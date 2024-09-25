{-# LANGUAGE OverloadedStrings #-}

module FlexTask.Processing.Text
  ( argDelimiter
  , formatAnswer
  , formatIfFlexSubmission
  , formatForJS
  , inputEscape
  , listDelimiter
  , removeUnicodeEscape
  ) where


import Data.Char  (isAscii, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text  (Text)
import Numeric    (showHex)
import Text.Read  (readMaybe)

import qualified Data.Text as T




argDelimiter :: Text
argDelimiter = "\a\a"

listDelimiter :: Text
listDelimiter = "\b\b"

inputEscape :: Text
inputEscape = "\""



escape :: Text -> Text
escape t = inputEscape <> T.pack (show t) <> inputEscape



process :: [Text] -> Text
process []   = escape "Missing"
process s    = T.intercalate listDelimiter $ map (escape . checkEmpty) s
  where
    checkEmpty t = if T.null t then "None" else t



formatAnswer :: [[Text]] -> Maybe Text
formatAnswer values
  | all null values = Nothing
  | otherwise = Just $ T.intercalate argDelimiter $ map process values


toJSUnicode :: Char -> Text
toJSUnicode c
      | not $ isAscii c = "\\u" <> T.justifyRight 4 '0' (T.pack $ showHex (fromEnum c) "")
      | otherwise       = T.singleton c



removeEscape :: Text -> [[Text]]
removeEscape t = map (\i -> fromMaybe i $ readMaybe $ T.unpack $ T.init $ T.tail i) <$> splitArgs t
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



formatForJS :: Text -> Text
formatForJS t = correctUnicodeEscape $ T.pack $ show $ asUnicode t



removeUnicodeEscape :: String -> String
removeUnicodeEscape (a:b:xs)
    | a == '\\' && isDigit b   = b : removeUnicodeEscape xs
    | otherwise                = a : removeUnicodeEscape (b:xs)
removeUnicodeEscape xs         = xs



-- Stopgap solution until Autotool frontend is updated to allow for display of task form in comments
formatIfFlexSubmission :: Text -> Text
formatIfFlexSubmission t
    | not (T.isInfixOf argDelimiter t) = t
    | null splitArgs || any T.null splitArgs = ""
    | otherwise = T.unlines numberInputs
    where
      splitArgs = T.splitOn argDelimiter t
      unescaped = map (read . T.unpack . T.init . T.tail) . T.splitOn listDelimiter <$> splitArgs
      fieldIndices = map (\i -> "Field " <> T.pack (show @Int i) <> ": ") [1..]
      numberInputs = zipWith (<>) fieldIndices $ map (T.intercalate ",") unescaped
