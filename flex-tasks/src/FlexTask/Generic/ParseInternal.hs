{-# language ApplicativeDo #-}
{-# language DefaultSignatures #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}


module FlexTask.Generic.ParseInternal
  ( Parse(..)
  , parseInstanceSingleChoice
  , parseInstanceMultiChoice
  , escaped
  , parseWithOrReport
  , reportWithFieldNumber
  , parseWithFallback
  , displayInputAnd
  ) where


import Control.Monad      (void)
import Control.OutputCapable.Blocks (
  LangM,
  LangM',
  OutputCapable,
  ReportT,
  english,
  german,
  indent,
  text,
  translate,
  )
import Control.OutputCapable.Blocks.Generic (
  toAbort,
  )
import Data.Text          (Text)
import GHC.Generics       (Generic(..), K1(..), M1(..), (:*:)(..))
import Text.Parsec
  ( ParseError
  , (<|>)
  , between
  , lookAhead
  , manyTill
  , many1
  , notFollowedBy
  , optionMaybe
  , parse
  , sepBy
  , sourceColumn
  , try
  )
import Text.Parsec.Char   (anyChar, char, digit, string)
import Text.Parsec.Error (
  errorMessages,
  errorPos,
  showErrorMessages,
  )
import Text.Parsec.String (Parser)
import Yesod              (Textarea(..))

import qualified Data.Text    as T

import FlexTask.Processing.Text (
  argDelimiter,
  emptyMarker,
  inputEscape,
  listDelimiter,
  missingMarker
  )
import FlexTask.Generic.Form
  ( MultipleChoiceSelection
  , SingleChoiceSelection
  , multipleChoiceAnswer
  , singleChoiceAnswer
  )




{- |
Class for generic parsing of types.
Bodyless instances can be declared for any type instancing Generic.
__Exception: Types with multiple constructors.__ Use utility functions for those or provide your own instance.
-}
class Parse a where
  formParser :: Parser a

  default formParser :: (Generic a, GParse (Rep a)) => Parser a
  formParser = to <$> gparse



class GParse f where
  gparse :: Parser (f a)



instance (GParse a, GParse b) => GParse (a :*: b) where
  gparse = do
    a <- gparse
    void $ parseText argDelimiter
    b <- gparse
    pure (a :*: b)



-- | Meta-information (constructor names, etc.)
instance GParse a => GParse (M1 i c a) where
  gparse = M1 <$> gparse



-- | Constants, additional parameters and recursion of kind *
instance Parse a => GParse (K1 i a) where
  gparse = K1 <$> formParser



instance Parse Int where
  formParser = escaped $ do
    sign <- optionMaybe $ char '-'
    ds <- many1 digit
    pure $ read $ case sign of
      Nothing -> ds
      Just s  -> s : ds



instance Parse String where
  formParser = escaped $ manyTill anyChar $ try $ lookAhead $
      escape >> notFollowedBy (string "\"")



instance Parse Text where
  formParser = T.pack <$> formParser



instance Parse Textarea where
  formParser = Textarea <$> formParser



instance Parse Bool where
  formParser = escaped $ do
    val <- try (string "yes") <|> string "no"
    pure $ case val of
            "yes" -> True
            _     -> False



instance Parse Double where
  formParser = escaped $ do
    sign <- optionMaybe $ char '-'
    whole <- many1 digit
    dot <- optionMaybe (char '.' <|> char ',')
    frac <- case dot of
      Nothing -> pure []
      Just _  -> ('.':) <$> do
        num <- many1 digit
        maybe num (num ++) <$> optionMaybe eParser
    pure $ read $ case sign of
      Nothing ->     whole
      Just s  -> s : whole
      ++ frac
    where
      eParser = (++) <$> string "e-" <*> many1 digit


instance (Parse a, Parse b) => Parse (a,b)

instance (Parse a, Parse b, Parse c) => Parse (a,b,c)

instance (Parse a, Parse b, Parse c, Parse d) => Parse (a,b,c,d)

instance (Parse a, Parse b, Parse c, Parse d, Parse e) => Parse (a,b,c,d,e)

instance (Parse a, Parse b, Parse c, Parse d, Parse e, Parse f) => Parse (a,b,c,d,e,f)



parseList :: Parse a => Parser [a]
parseList = try (escaped parseEmpty) <|> sepBy formParser (parseText listDelimiter)
    where
      parseEmpty = parseText missingMarker >> pure []


instance {-# Overlappable #-} Parse a => Parse [a] where
  formParser = parseList


-- To avoid clash with TypeError instance in Parse.hs
instance Parse [String] where
  formParser = parseList


instance Parse a => Parse (Maybe a) where
  formParser = do
    mValue <- optionMaybe $ try $ escaped $ parseText emptyMarker
    case mValue of
      Nothing -> Just <$> formParser
      Just _  -> pure Nothing


instance Parse SingleChoiceSelection where
  formParser = singleChoiceAnswer <$> formParser


instance Parse MultipleChoiceSelection where
  formParser = multipleChoiceAnswer <$> parseWithEmptyMarker



{- |
Parser for single choice answer of Enum types. Use as implementation of `formParser` for manual `Parse` instances.
Intended for use with types such as

@
data MyType = One | Two | Three deriving (Bounded, Enum, Eq)
@

that can not use a bodyless `Parse` instance.
-}
parseInstanceSingleChoice :: (Bounded a, Enum a, Eq a) => Parser a
parseInstanceSingleChoice = toEnum . subtract 1 <$> formParser



-- | Same as `parseInstanceSingleChoice`, but for parsing a List of the given type, i.e. a multiple choice version.
parseInstanceMultiChoice :: (Bounded a, Enum a, Eq a) => Parser [a]
parseInstanceMultiChoice = map (toEnum . subtract 1) <$> parseWithEmptyMarker



parseWithEmptyMarker :: Parser [Int]
parseWithEmptyMarker = filter (>0) <$> formParser



escape :: Parser String
escape = esc >> esc
  where esc = parseText inputEscape



{- |
Parses FlexTask answer escape characters enclosing the input.
Use this to wrap preexisting parsers that are used to parse a student solution.
Otherwise your parser will fail on the escape characters.
Parsers generated by `Parse` already consider the escaping and do not need to be wrapped.
-}
escaped :: Parser a -> Parser a
escaped = between escape escape



parseText :: Text -> Parser String
parseText t = string $ T.unpack t



{- |
Parses a String with the given parser and embeds the result into the `OutputCapable` interface.
No value will be embedded in case of a `ParseError`. Instead, an error report is given then.
That report is built using the second function argument.
The report will automatically abort after displaying.
It is therefore not necessary to include a `refuse`, but it is not harmful either.
Adding a refuse will display text and cut off any following output as usual.
This can be useful for giving better error messages.
-}
parseWithOrReport ::
  (Monad m, OutputCapable (ReportT o m))
  => Parser a
  -> (String -> ParseError -> LangM (ReportT o m))
  -> String
  -> LangM' (ReportT o m) a
parseWithOrReport parser errorMsg answer =
  case parse parser "" answer of
    Left failure  -> toAbort $ errorMsg answer failure
    Right success -> pure success


{- |
Parses a String with the given parser.
Allows for further processing of a possible parse error.
A second parser is used as a fallback in case of an error.
The result of both parsers is then used to construct the report.
Comments on `refuse`'s behaviour for `parseWithOrReport` also apply for this function.
This can be useful for giving more specific error messages,
e.g. checking a term for bracket consistency even if the parser failed early on.
-}
parseWithFallback ::
  (Monad m, OutputCapable (ReportT o m))
  => Parser a
  -- ^ Parser to use initially
  -> (String -> Maybe ParseError -> ParseError -> LangM (ReportT o m))
  -- ^ How to produce an error report based on:
  -- ^ 1. The input string
  -- ^ 2. The possible parse error of the fallback parser
  -- ^ 3. The original parse error
  -> Parser ()
  -- ^ The secondary parser to use in case of a parse error.
  -- ^ Only used for generating possible further errors, thus does not return a value.
  -> String
  -- ^ The input
  -> LangM' (ReportT o m) a
  -- ^ The finished error report or embedded value
parseWithFallback parser messaging fallBackParser =
  parseWithOrReport
    parser
    (\a -> messaging a (either Just (const Nothing) (parse fallBackParser "" a)))



{- |
Provide error report with positional information relative to an input form.
-}
reportWithFieldNumber :: OutputCapable m => String -> ParseError -> LangM m
reportWithFieldNumber input e = do
    translate $ do
      german $ "Fehler in Eingabefeld" ++ " " ++ fieldNum ++ ":"
      english $ "Error in input field" ++ " " ++ fieldNum ++ ":"
    indent $ text errors
    pure ()
  where
    fieldNum = show $ length (filter (=='\a') consumed) `div` 2 + 1
    errors = showErrorMessages
      "or"
      "unknown parse error"
      "expecting"
      "unexpected"
      "end of input"
      $ errorMessages e
    consumed = take (sourceColumn $ errorPos e) input

displayInputAnd ::
  OutputCapable m =>
  (Maybe a -> ParseError -> LangM m)
  -> String -> Maybe a -> ParseError -> LangM m
displayInputAnd messaging a ma err = do
  translate $ do
    german $ "Fehler in" ++  " \"" ++ a ++ "\" : "
    english $ "Error in" ++ " \"" ++ a ++ "\" : "
  indent $ messaging ma err
  pure ()
