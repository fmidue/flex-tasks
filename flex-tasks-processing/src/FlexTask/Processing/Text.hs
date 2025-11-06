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
    -- * Form Duplication for Autotool Comment View
  , uniqueFormCopy
    -- * Internationalization
  , supportedLanguages
  ) where


import Data.Char                        (isAscii, isDigit)
import Data.List.Extra                  (replace)
import Data.Maybe                       (fromMaybe)
import Data.Text                        (Text)
import Numeric                          (showHex)
import Text.Blaze.Html                  (Html, preEscapedToHtml)
import Text.Blaze.Html.Renderer.String  (renderHtml)
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
formatAnswer :: [[[Text]]] -> Maybe Text
formatAnswer values
  | all (all null) values = Nothing
  | otherwise = Just $ T.intercalate argDelimiter $ map (process . concat) values


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



{- |
Create an exact duplicate of the given form data, but append all field names with a unique identifier.
This is used to render multiple views of the input form on the same page, e.g. the comments page in Autotool.
-}
uniqueFormCopy :: ([[Text]],Html) -> String -> ([[Text]],Html)
uniqueFormCopy (params,html) uniqueId = (newParams, alteredHtml)
  where
    suffix = '-' : uniqueId
    newParams = map (map (<> T.pack suffix)) params
    alteredHtml = preEscapedToHtml $ foldr
      ( (\param -> replace ("name=\""<> param) ("name=\""<> param <> suffix)) .
        T.unpack
      )
      (renderHtml html)
      $ concat params


{- |
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
      escapeSeq = inputEscape <> inputEscape
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
