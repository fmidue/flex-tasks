{-# language DefaultSignatures #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}


module FlexTask.Generic.ParseInternal
  ( Parse(..)
  , parseInstanceSingleChoice
  , parseInstanceMultiChoice
  , escaped
  , useParser
  ) where


import Control.Monad      (void)
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
  , try
  )
import Text.Parsec.Char   (anyChar, char, digit, string)
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
  parseInput :: Parser a

  default parseInput :: (Generic a, GParse (Rep a)) => Parser a
  parseInput = to <$> gparse



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
  gparse = K1 <$> parseInput



instance Parse Int where
  parseInput = escaped $ do
    sign <- optionMaybe $ char '-'
    ds <- many1 digit
    pure $ read $ case sign of
      Nothing -> ds
      Just s  -> s : ds



instance Parse String where
  parseInput = escaped $ manyTill anyChar $ try $ lookAhead $
      escape >> notFollowedBy (string "\"")



instance Parse Text where
  parseInput = T.pack <$> parseInput



instance Parse Textarea where
  parseInput = Textarea <$> parseInput



instance Parse Bool where
  parseInput = escaped $ do
    val <- try (string "yes") <|> string "no"
    pure $ case val of
            "yes" -> True
            _     -> False



instance Parse Double where
  parseInput = escaped $ do
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
parseList = try (escaped parseEmpty) <|> sepBy parseInput (parseText listDelimiter)
    where
      parseEmpty = parseText missingMarker >> pure []


instance {-# Overlappable #-} Parse a => Parse [a] where
  parseInput = parseList


-- To avoid clash with TypeError instance in Parse.hs
instance Parse [String] where
  parseInput = parseList


instance Parse a => Parse (Maybe a) where
  parseInput = do
    mValue <- optionMaybe $ try $ escaped $ parseText emptyMarker
    case mValue of
      Nothing -> Just <$> parseInput
      Just _  -> pure Nothing


instance Parse SingleChoiceSelection where
  parseInput = singleChoiceAnswer <$> parseInput


instance Parse MultipleChoiceSelection where
  parseInput = multipleChoiceAnswer <$> parseInput



{- |
Parser for single choice answer of Enum types. Use as implementation of `parseInput` for manual `Parse` instances.
Intended for use with types such as

@
data MyType = One | Two | Three deriving (Bounded, Enum, Eq)
@

that can not use a bodyless `Parse` instance.
-}
parseInstanceSingleChoice :: (Bounded a, Enum a, Eq a) => Parser a
parseInstanceSingleChoice = toEnum . subtract 1 <$> parseInput



-- | Same as `parseInstanceSingleChoice`, but for parsing a List of the given type, i.e. a multiple choice version.
parseInstanceMultiChoice :: (Bounded a, Enum a, Eq a) => Parser [a]
parseInstanceMultiChoice = map (toEnum . subtract 1) <$> parseInput



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



useParser :: Parser a -> String -> Either ParseError a
useParser p = parse p ""
