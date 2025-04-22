{-# LANGUAGE OverloadedStrings #-}

{- |
Various text processing functions used to format input for display.
-}

module FlexTask.Processing.Text
  ( -- * Control Sequences
    -- $control
    argDelimiter
  , listDelimiter
  , inputEscape
  , missingMarker
  , emptyMarker
    -- * Formatting Functions
  , formatAnswer
  , formatIfFlexSubmission
  , formatForJS
  , removeUnicodeEscape
    -- * Internationalization
  , supportedLanguages
  ) where


import Data.Char                        (isAscii, isDigit)
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
argDelimiter = "\a\a"

-- | Inner delimiter for elements of a field list.
listDelimiter :: Text
listDelimiter = "\b\b"

-- | Sequence denoting the start and end of a fields value.
inputEscape :: Text
inputEscape = "\""

-- | Marker for a missing field
missingMarker :: Text
missingMarker = "Missing"

-- | Marker for a blank optional field
emptyMarker :: Text
emptyMarker = "None"


escape :: Text -> Text
escape t = inputEscape <> T.pack (show t) <> inputEscape



process :: [Text] -> Text
process []   = escape "Missing"
process s    = T.intercalate listDelimiter $ map (escape . checkEmpty) s
  where
    checkEmpty t = if T.null t then "None" else t



-- | format a list of (nested) individual answers into a single answer String
formatAnswer :: [[Text]] -> Maybe Text
formatAnswer values
  | all null values = Nothing
  | otherwise = Just $ T.intercalate argDelimiter $ map process values


toJSUnicode :: Char -> Text
toJSUnicode c
      | not $ isAscii c = "\\u" <> T.justifyRight 4 '0' (T.pack $ showHex (fromEnum c) "")
      | otherwise       = T.singleton c



removeEscape :: Text -> [[Text]]
removeEscape t = map (\i -> fromMaybe i $ readMaybe $ T.unpack $ T.drop 1 $ T.dropEnd 1 i) <$> splitArgs t
  where
    splitArgs = map (T.splitOn listDelimiter) . T.splitOn argDelimiter



asUnicode :: Text -> [Text]
asUnicode t = compress . map (T.concatMap toJSUnicode) <$> removeEscape t
  where
    compress x
      | length x > 1 = T.pack $ show x
      | otherwise    = T.concat x


correctUnicodeEscape :: Text -> Text
correctUnicodeEscape t = T.replace "\\\\\\u" "\\\\u" stepOne
  where
    stepOne = T.replace "\\\\u" "\\u" t



-- | Process Text containing Haskell Unicode representation for use in JavaScript.
formatForJS :: Text -> Text
formatForJS t = correctUnicodeEscape $ T.pack $ show $ asUnicode t



{- |
Remove excessive escape characters in front of Unicode
caused by conversion between Haskell and JavaScript representation.
-}
removeUnicodeEscape :: String -> String
removeUnicodeEscape (x:xs)
    | x == '\\' && ident > 127 && inUnicodeRange
      = unicodeIdent ++ removeUnicodeEscape rest
    | otherwise = x : removeUnicodeEscape xs
  where
    (unicodeIdent,rest) = span isDigit xs
    ident = fromMaybe 0 $ readMaybe unicodeIdent
    inUnicodeRange = ident <= (1114111 :: Int)
removeUnicodeEscape xs = xs



-- Stopgap solution until Autotool frontend is updated to allow for display of task form in comments
{- |
Format an answer String into a vertical text listing of individual values.

__temporary solution: Will be replaced in future versions.__
-}
formatIfFlexSubmission :: Text -> Text
formatIfFlexSubmission t
    | not ( argDelimiter  `T.isInfixOf` t ||
            listDelimiter `T.isInfixOf` t ||
            escapeWrapped
          ) = t
    | length splitArgs == 1 = stripEscape t
    | null splitArgs || any T.null splitArgs = ""
    | otherwise = T.unlines numberInputs
    where
      escapeSeq = inputEscape <> inputEscape
      escapeWrapped = escapeSeq `T.isPrefixOf` t && escapeSeq `T.isSuffixOf` t
      splitArgs = T.splitOn argDelimiter t
      stripEscape = read . T.unpack . T.drop 1 . T.dropEnd 1
      unescaped = map stripEscape . T.splitOn listDelimiter <$> splitArgs
      fieldIndices = map (\i -> "Field " <> T.pack (show @Int i) <> ": ") [1..]
      numberInputs = zipWith (<>) fieldIndices $ map (T.intercalate ",") unescaped



-- | List of languages to cover for input form HTML in instances of `RenderMessage` for custom translations.
supportedLanguages :: [Lang]
supportedLanguages = ["de","en"]
