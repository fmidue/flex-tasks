{-# LANGUAGE OverloadedStrings #-}

{- |
Various text processing functions used to format input for display.
-}

module FlexTask.Processing.Text
  ( -- * Control Sequences
    -- $control
    argDelimiter
  , listDelimiters
  , missingMarker
  , emptyMarker
  , plaintextTag
    -- * Formatting Functions
  , formatAnswer
  , formatIfFlexSubmission
  , formatForJS
  , submissionToJs
  , processParam
    -- * Internationalization
  , supportedLanguages
  ) where

import Data.Char                        (isAscii)
import Data.List.Extra                  (intercalate, replace, singleton)
import Data.Text                        (Text)
import Data.Tuple.Extra                 (both)
import Numeric                          (showHex)
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Language (
  haskell,
  )
import Text.ParserCombinators.Parsec.Token (
  stringLiteral,
  )
import Text.Shakespeare.I18N            (Lang)
import TextShow                         (showt)

import qualified Data.Text as T



{- $control
Student answers for FlexTasks are compiled into a single String after retrieval.
The answer String contains control sequences which encode the structure of the input form.
-}

-- | Outer delimiter for individual fields.
argDelimiter :: Text
argDelimiter = ","

-- | Inner delimiter for elements of a field list.
listDelimiters :: (Text,Text)
listDelimiters = ("[","]")

-- | Marker for a missing field
missingMarker :: Text
missingMarker = "Missing"

-- | Marker for a blank optional field
emptyMarker :: Text
emptyMarker = "Nothing"


-- | tag for identifying plaintext fields
plaintextTag :: Text
plaintextTag = "-plaintext"



{- | pre-process POST parameters:
  1. Mark empty submissions as optional values instead of an empty String.
  2. Escape plaintext inputs with double quotes for easier parsing.
-}
processParam :: Functor f => (Text -> f [Text]) -> Text -> f [Text]
processParam getValues param = map decide <$> getValues param
  where
    decide :: Text -> Text
    decide t
      | T.null t = emptyMarker
      | plaintextTag `T.isSuffixOf` param = showt t
      | otherwise = t


-- | format a list of (nested) individual answers into a single answer String
formatAnswer :: [[[Text]]] -> Maybe Text
formatAnswer values
  | all (all null) values = Nothing
  | otherwise = Just $ T.intercalate argDelimiter $ map (processList . concat) values
  where
    (open,close) = listDelimiters

    processList []  = missingMarker
    processList [x] = x
    processList s   = open <> T.intercalate argDelimiter s <> close


toJSUnicode :: Char -> String
toJSUnicode c
      | not $ isAscii c = "\\u" <> justifyRight 4 (showHex (fromEnum c) "")
      | otherwise       = singleton c
  where
    justifyRight n s = replicate (n - length s) '0' ++ s


submissionToJs :: Parser [String]
submissionToJs = sepBy1 (parseString <|> parseList <|> parseOther) (char ',')
  where
    (open,close) = both T.unpack listDelimiters

    parseString :: Parser String
    parseString = concatMap toJSUnicode <$> stringLiteral haskell

    parseList :: Parser String
    parseList = do
      xs <- between (string open) (string close) $
        sepBy1 (parseString <|> parseOther) (string $ T.unpack argDelimiter)
      pure $ open ++ intercalate "," (map show xs) ++ close

    parseOther :: Parser String
    parseOther = many1 (noneOf "\"[,]")


-- | Process Text containing Haskell Unicode representation for use in JavaScript.
formatForJS :: Text -> Text
formatForJS t = case parse submissionToJs "" (T.unpack t) of
    Left e   -> T.pack $ show e -- no TextShow instance
    Right xs -> showt $ map correctUnicodeEscape xs
  where correctUnicodeEscape = replace "\\\\u" "\\u"


{- |
FIX: THIS NO LONGER MAKES ANY SENSE. REPLACE AFTER FINISHING REWRITE

Format an answer String into a vertical text listing of individual values.
This is used to display Flex submissions in a non-HTML context, e.g. in a downloadable text file.
-}
formatIfFlexSubmission :: Text -> Text
formatIfFlexSubmission = id
{-
    | not ( argDelimiter  `T.isInfixOf` t ||
            listDelimiter `T.isInfixOf` t ||
            escapeWrapped
          ) = t
    | length splitArgs == 1 && length splitLists == 1 = stripEscape t
    | null splitArgs || any T.null splitArgs = ""
    | otherwise = T.unlines numberInputs
    where
      escapeSeq = ""
      escapeWrapped = escapeSeq `T.isPrefixOf` t && escapeSeq `T.isSuffixOf` t
      splitArgs = T.splitOn argDelimiter t
      splitLists = T.splitOn listDelimiter t
      stripEscape = fromMaybe failureMessage . readMaybe . T.unpack . T.drop 1 . T.dropEnd 1
      unescaped = map stripEscape . T.splitOn listDelimiter <$> splitArgs
      fieldIndices = map (\i -> "Field " <> T.pack (show @Int i) <> ": ") [1..]
      numberInputs = zipWith (<>) fieldIndices $ map (T.intercalate ",") unescaped
      failureMessage = "failed to format value for display"
-}

-- | List of languages to cover for input form HTML in instances of `RenderMessage` for custom translations.
supportedLanguages :: [Lang]
supportedLanguages = ["de","en"]
