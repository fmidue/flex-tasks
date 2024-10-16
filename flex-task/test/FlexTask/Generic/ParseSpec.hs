{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module FlexTask.Generic.ParseSpec where


import Data.List.Extra                  (dropEnd)
import Data.Maybe                       (fromMaybe)
import Data.Text                        (pack, unpack)
import Test.Hspec (
  Spec,
  context,
  describe,
  specify,
  pendingWith,
  )
import Test.Hspec.QuickCheck            (prop)
import Test.Hspec.Parsec                (shouldFailOn, shouldParse)
import Test.QuickCheck                  (chooseInt, forAll, sublistOf)
import Test.QuickCheck.Instances.Text   ()
import Text.Parsec                      (eof, digit, many1)
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
      case useParser (testParse <* eof) s of
        Left _ -> useParser (escaped testParse) `shouldFailOn` escapedSingle s
        Right res -> useParser (escaped testParse) (escapedSingle s) `shouldParse` stripEscape res

  describe "parseInput" $ do
    context "should work for all base types" $ do
      prop "String" $ testParsingString id
      prop "Text" $ testParsingString pack
      prop "Textarea" $ testParsingString $ Textarea . pack
      prop "Bool" $ testParsing boolShow
      prop "Int" $ testParsing @Int show
      prop "Double" $ testParsing @Double show

    context "should work for lists" $ do
      prop "String" $ pendingWith "prevented by nested lists type error. fix first!"
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
      useParser parseInput (escapedSingle $ show i) `shouldParse` singleChoiceAnswer i
    prop "multiple choice works" $ \is ->
      useParser parseInput (escapedList $ map show is) `shouldParse` multipleChoiceAnswer is

  describe "choice selection parsers (for a test enum)" $ do
    specify "single choice works" $
      forAll (chooseInt (0,2)) $ \i ->
        useParser parseInput (escapedSingle $ show $ i+1) `shouldParse` toEnum @TestEnum i
    specify "multiple choice works" $
      forAll (sublistOf [0..2]) $ \is ->
        useParser parseInput (escapedList $ map (show . (+1)) is) `shouldParse` map (toEnum @TestEnum) is
  where
    testParse = many1 digit
    boolShow b = if b then "yes" else "no"
    maybeShow = maybe "None"

    testParsingMaybeStringList fromString = testParsingStringList (with fromString)
    testParsingMaybe from = testParsingString (with from)


testParsingString :: (Eq a, Parse a, Show a) => (String -> a) -> String -> IO ()
testParsingString fromString s = useParser parseInput (escapedSingle s) `shouldParse` fromString (stripEscape s)


testParsing :: (Eq a, Parse a, Show a) => (a -> String) -> a -> IO ()
testParsing toString a = useParser parseInput (escapedSingle $ toString a) `shouldParse` a


testParsingList :: (Eq a, Show a, Parse [a]) => (a -> String) -> [a] -> IO ()
testParsingList toString as = useParser parseInput (escapedList $ map toString as) `shouldParse` as


testParsingStringList :: (Eq a, Parse [a], Show a) => (String -> a) -> [String] -> IO ()
testParsingStringList fromString s = useParser parseInput (escapedList s) `shouldParse` map (fromString . stripEscape) s


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


with :: (String -> a) -> String -> Maybe a
with from s
  | s == "None" = Nothing
  | otherwise =  Just $ from s
