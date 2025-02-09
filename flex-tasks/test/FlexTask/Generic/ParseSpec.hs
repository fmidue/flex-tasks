{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module FlexTask.Generic.ParseSpec where


import Data.List.Extra                  (dropEnd)
import Data.Maybe                       (fromMaybe)
import Data.Text                        (pack, unpack)
import Test.Hspec (
  Expectation,
  Spec,
  context,
  describe,
  specify,
  )
import Test.Hspec.QuickCheck            (prop)
import Test.Hspec.Parsec                (shouldFailOn, shouldParse)
import Test.QuickCheck                  (chooseInt, forAll, sublistOf)
import Test.QuickCheck.Instances.Text   ()
import Text.Parsec                      (ParseError, eof, digit, many1, parse)
import Text.Parsec.String               (Parser)
import Yesod (Textarea(..))

import FlexTask.Generic.Form (
  singleChoiceAnswer,
  multipleChoiceAnswer,
  )
import FlexTask.Generic.Parse
import FlexTask.Processing.Text         (formatAnswer)


data TestEnum = One | Two | Three deriving (Bounded, Enum, Eq, Show)

instance Parse TestEnum where
  parseInput = parseInstanceSingleChoice

instance Parse [TestEnum] where
  parseInput = parseInstanceMultiChoice


spec :: Spec
spec = do
  describe "escaped" $
    prop "works like the original parser, minding the input escape" $ \s ->
      case withParser (testParse <* eof) s of
        Left _ -> withParser (escaped testParse) `shouldFailOn` escapedSingle s
        Right res -> withParser (escaped testParse) (escapedSingle s) `shouldParse` stripEscape res

  describe "parseInput" $ do
    context "should work for all base types" $ do
      prop "String" $ testParsingString id
      prop "Text" $ testParsingString pack
      prop "Textarea" $ testParsingString $ Textarea . pack
      prop "Bool" $ testParsing boolShow
      prop "Int" $ testParsing @Int show
      prop "Double" $ testParsing @Double show

    context "should work for lists" $ do
      prop "String" $ testParsingStringList id
      prop "Text" $ testParsingStringList pack
      prop "Textarea" $ testParsingStringList (Textarea . pack)
      prop "Bool" $ testParsingList boolShow
      prop "Int" $ testParsingList @Int show
      prop "Double" $ testParsingList @Double show

    context "should work for optional values" $ do
      prop "String" $ testParsingMaybe id
      prop "Text" $ testParsingMaybe pack
      prop "Textarea" $ testParsingMaybe (Textarea . pack)
      prop "Bool" $ testParsing $ maybeShow boolShow
      prop "Int" $ testParsing @(Maybe Int) $ maybeShow show
      prop "Double" $ testParsing @(Maybe Double) $ maybeShow show

    context "should work for lists of optional values" $ do
      prop "String" $ testParsingMaybeStringList id
      prop "Text" $ testParsingMaybeStringList pack
      prop "Textarea" $ testParsingMaybeStringList (Textarea . pack)
      prop "Bool" $ testParsingList $ maybeShow boolShow
      prop "Int" $ testParsingList @(Maybe Int) $ maybeShow show
      prop "Double" $ testParsingList @(Maybe Double) $ maybeShow show

  describe "anonymous choice selection parsers" $ do
    prop "single choice works" $ \i ->
      escapedSingle (show i) `parsesTo` singleChoiceAnswer i
    prop "multiple choice works" $ \is ->
      escapedList (map show is) `parsesTo` multipleChoiceAnswer (removeEmpty is)

  describe "choice selection parsers (for a test enum)" $ do
    specify "single choice works" $
      forAll (chooseInt (0,2)) $ \i ->
        escapedSingle (show $ i+1) `parsesTo` toEnum @TestEnum i
    specify "multiple choice works" $
      forAll (sublistOf [0..2]) $ \is ->
        escapedList (map (show . (+1)) is) `parsesTo` map (toEnum @TestEnum) (removeEmpty is)
  where
    testParse = many1 digit
    boolShow b = if b then "yes" else "no"
    maybeShow = maybe "None"
    removeEmpty = filter (<0)

    testParsingMaybeStringList fromString = testParsingStringList (format fromString)
    testParsingMaybe from = testParsingString (format from)


testParsingString :: (Eq a, Parse a, Show a) => (String -> a) -> String -> IO ()
testParsingString fromString s = escapedSingle s `parsesTo` fromString (stripEscape s)


testParsing :: (Eq a, Parse a, Show a) => (a -> String) -> a -> IO ()
testParsing toString a = escapedSingle (toString a) `parsesTo` a


testParsingList :: (Eq a, Show a, Parse [a]) => (a -> String) -> [a] -> IO ()
testParsingList toString as = escapedList (map toString as) `parsesTo` as


testParsingStringList :: (Eq a, Parse [a], Show a) => (String -> a) -> [String] -> IO ()
testParsingStringList fromString s = escapedList s `parsesTo` map (fromString . stripEscape) s


escapedSingle :: String -> String
escapedSingle = escapedList . (:[])


escapedList :: [String] -> String
escapedList = escapedString . (:[])


escapedString :: [[String]] -> String
escapedString = unpack . fromMaybe "" . formatAnswer . map (map pack)


stripEscape :: String -> String
stripEscape = toMaybe . drop 1 . dropEnd 1 . show
  where
    toMaybe s = if null s then "None" else s


format :: (String -> a) -> String -> Maybe a
format from s
  | s == "None" = Nothing
  | otherwise =  Just $ from s


withParser :: Parser a -> String -> Either ParseError a
withParser p = parse p ""


parsesTo :: (Show a, Eq a, Parse a) => String -> a -> Expectation
parsesTo input output = withParser parseInput input `shouldParse` output
