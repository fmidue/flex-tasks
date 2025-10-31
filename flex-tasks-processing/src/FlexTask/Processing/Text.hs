{-# LANGUAGE OverloadedStrings #-}

{- |
Various text processing functions used to format input for display.
-}

module FlexTask.Processing.Text
  ( -- * Control Sequences
    -- $control
    argDelimiter
  , listDelimiter
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

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Language (
  haskell,
  )
import Text.ParserCombinators.Parsec.Token (
  stringLiteral,
  )

import Data.Char                        (isAscii)
import Data.List                        (intercalate)
import Data.Maybe                       (fromMaybe)
import Data.Text                        (Text)
import Numeric                          (showHex)
import Text.Read                        (readMaybe)
import Text.Shakespeare.I18N            (Lang)

import qualified Data.Text as T



{- $control
Student answers for FlexTasks are compiled into a single String after retrieval.
The answer String contains control sequences which encode the structure of the input form.
-}

-- | Outer delimiter for individual fields.
argDelimiter :: Text
argDelimiter = ","

-- | Inner delimiter for elements of a field list.
listDelimiter :: Text
listDelimiter = ","

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
      | plaintextTag `T.isSuffixOf` param = T.pack $ show t
      | otherwise = t


-- | format a list of (nested) individual answers into a single answer String
formatAnswer :: [[[Text]]] -> Maybe Text
formatAnswer values
  | all (all null) values = Nothing
  | otherwise = Just $ T.intercalate argDelimiter $ map (processList . concat) values
  where
    processList []  = missingMarker
    processList [x] = x
    processList s   = "[" <> T.intercalate listDelimiter s <> "]"


toJSUnicode :: Char -> Text
toJSUnicode c
      | not $ isAscii c = "\\u" <> T.justifyRight 4 '0' (T.pack $ showHex (fromEnum c) "")
      | otherwise       = T.singleton c



asUnicode :: Text -> Text
asUnicode = T.concatMap toJSUnicode



correctUnicodeEscape :: Text -> Text
correctUnicodeEscape t = stepOne
  where
    stepOne = T.replace "\\\\u" "\\u" t


submissionToJs :: Parser [String]
submissionToJs = sepBy1 (parseString <|> parseList <|> parseOther) (char ',')
  where
    parseString :: Parser String
    parseString = stringLiteral haskell

    parseList :: Parser String
    parseList = do
      xs <- between (char '[') (char ']') (sepBy1 (parseString <|> parseOther) (char ','))
      pure $ "[" ++ intercalate "," (map (show . asUnicode . T.pack) xs) ++ "]"

    parseOther :: Parser String
    parseOther = many1 (noneOf "\"[,]")


-- | Process Text containing Haskell Unicode representation for use in JavaScript.
formatForJS :: Text -> [Text]
formatForJS t = case parse submissionToJs "" (T.unpack t) of
  Left e -> [T.pack $ show e]
  Right xs -> map (correctUnicodeEscape . T.pack) xs


{- |
FIX: THIS NO LONGER MAKES ANY SENSE. REPLACE AFTER FINISHING REWRITE

Format an answer String into a vertical text listing of individual values.
This is used to display Flex submissions in a non-HTML context, e.g. in a downloadable text file.
-}
formatIfFlexSubmission :: Text -> Text
formatIfFlexSubmission t
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


-- | List of languages to cover for input form HTML in instances of `RenderMessage` for custom translations.
supportedLanguages :: [Lang]
supportedLanguages = ["de","en"]
