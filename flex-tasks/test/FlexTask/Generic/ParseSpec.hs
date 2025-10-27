{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module FlexTask.Generic.ParseSpec where


import Data.List.Extra                  (intercalate)
import Data.Text                        (pack)
import Test.Hspec (
  Expectation,
  Spec,
  context,
  describe,
  expectationFailure,
  specify,
  shouldSatisfy,
  )
import Test.Hspec.QuickCheck            (prop)
import Test.Hspec.Parsec                (shouldParse)
import Test.QuickCheck                  (chooseInt, forAll, sublistOf)
import Test.QuickCheck.Instances.Text   ()
import Text.Parsec                      (ParseError, parse)
import Text.Parsec.String               (Parser)
import Yesod (Textarea(..))

import FlexTask.Generic.Form (
  singleChoiceAnswer,
  multipleChoiceAnswer,
  )
import FlexTask.Generic.Parse


data TestEnum = One | Two | Three deriving (Bounded, Enum, Eq, Show)

instance Parse TestEnum where
  formParser = parseInstanceSingleChoice

instance Parse [TestEnum] where
  formParser = parseInstanceMultiChoice


spec :: Spec
spec = do
  describe "formParser" $ do
    context "should work for all base types" $ do
      prop "String" $ testParsingString id
      prop "Text" $ testParsingString pack
      prop "Textarea" $ testParsingString $ Textarea . pack
      prop "Bool" $ testParsing boolShow
      prop "Int" $ testParsing @Int show
      prop "Double" $ \a -> parsesNear @Double (show a) a $ doubleInaccuracy a

    context "should work for lists" $ do
      prop "String" $ testParsingStringList id
      prop "Text" $ testParsingStringList pack
      prop "Textarea" $ testParsingStringList (Textarea . pack)
      prop "Bool" $ testParsingList boolShow
      prop "Int" $ testParsingList @Int show
      prop "Double" $ \a -> parsesNear @[Double] (toShowList show a) a $
        and . zipWith doubleInaccuracy a

    context "should work for optional values" $ do
      prop "String" $ testParsingMaybe id
      prop "Text" $ testParsingMaybe pack
      prop "Textarea" $ testParsingMaybe (Textarea . pack)
      prop "Bool" $ testParsing $ maybeShow boolShow
      prop "Int" $ testParsing @(Maybe Int) $ maybeShow show
      prop "Double" $ \a -> parsesNear @(Maybe Double) (maybeShow show a) a $
        compareMaybeDoubles a

    context "should work for lists of optional values" $ do
      prop "String" $ testParsingMaybeStringList id
      prop "Text" $ testParsingMaybeStringList pack
      prop "Textarea" $ testParsingMaybeStringList (Textarea . pack)
      prop "Bool" $ testParsingList $ maybeShow boolShow
      prop "Int" $ testParsingList @(Maybe Int) $ maybeShow show
      prop "Double" $ \a -> parsesNear @[Maybe Double] (toShowList (maybeShow show) a) a
        $ and . zipWith compareMaybeDoubles a

  describe "anonymous choice selection parsers" $ do
    prop "single choice works" $ \i ->
      show i `parsesTo` singleChoiceAnswer i
    prop "multiple choice works" $ \is ->
      toShowList show is `parsesTo` multipleChoiceAnswer (removeEmpty is)

  describe "choice selection parsers (for a test enum)" $ do
    specify "single choice works" $
      forAll (chooseInt (0,2)) $ \i ->
        show (i+1) `parsesTo` toEnum @TestEnum i
    specify "multiple choice works" $
      forAll (sublistOf [0..2]) $ \is ->
        toShowList show is `parsesTo`
        map (toEnum @TestEnum . subtract 1) (removeEmpty is)
  where
    boolShow b = if b then "yes" else "no"
    maybeShow = maybe "Nothing"
    removeEmpty = filter (>0)

    testParsingMaybeStringList fromString = testParsingStringList (format fromString)
    testParsingMaybe from = testParsingString (format from)
    compareMaybeDoubles a mRes = mRes == a || (doubleInaccuracy <$> a <*> mRes) == Just True

testParsingString :: (Eq a, Parse a, Show a) => (String -> a) -> String -> IO ()
testParsingString fromString s = show s `parsesTo` fromString s


testParsing :: (Eq a, Parse a, Show a) => (a -> String) -> a -> IO ()
testParsing toString a = toString a `parsesTo` a


testParsingList :: (Eq a, Show a, Parse [a]) => (a -> String) -> [a] -> IO ()
testParsingList toString as = toShowList toString as `parsesTo` as


testParsingStringList :: (Eq a, Parse [a], Show a) => (String -> a) -> [String] -> IO ()
testParsingStringList fromString s = show s `parsesTo` map fromString s


format :: (String -> a) -> String -> Maybe a
format from s
  | s == "Nothing" = Nothing
  | otherwise =  Just $ from s


withParser :: Parser a -> String -> Either ParseError a
withParser p = parse p ""


parsesTo :: (Show a, Eq a, Parse a) => String -> a -> Expectation
parsesTo input output = withParser formParser input `shouldParse` output


parsesNear :: (Show a, Parse a) => String -> a -> (a -> Bool) -> IO ()
parsesNear input = shouldParseWith $ withParser formParser input


shouldParseWith
  :: Show a
  => Either ParseError a
  -> a
  -> (a -> Bool)
  -> IO ()
shouldParseWith i o p = case i of
  Left e -> expectationFailure $ "expected: " ++ show o ++
    '\n' : "but parsing failed with error:\n" ++ show e
  Right x -> x `shouldSatisfy` p


doubleInaccuracy :: Double -> Double -> Bool
doubleInaccuracy a b = abs (a-b) < 0.001


toShowList :: (a -> String) -> [a] -> String
toShowList toString as = "[" ++ intercalate "," (map toString as) ++ "]"
