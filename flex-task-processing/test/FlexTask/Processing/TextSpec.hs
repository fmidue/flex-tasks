{-# language OverloadedStrings #-}

module FlexTask.Processing.TextSpec where


import Data.Char                        (isAscii)
import Data.List                        (intersperse)
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Text                        (Text, pack)
import Test.Hspec                       (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck            (prop)
import Test.QuickCheck                  (arbitrary, chooseInt, forAll, suchThat)
import Test.QuickCheck.Instances.Text   ()

import FlexTask.Processing.Text

import qualified Data.Text              as T



spec :: Spec
spec = do
  describe "formatAnswer" $ do
    it "returns Nothing if there is no input" $
      forAll (arbitrary `suchThat` all null) $ \tss ->
        formatAnswer tss `shouldBe` Nothing
    it "correctly encodes a simple unit test" $
      formatAnswer [["one"],[],[""],["two","three"]]
      `shouldBe`
      Just formatUnitTest
    it "inserts delimiters and marks input correctly" $
      forAll (arbitrary `suchThat` (not . all null)) $ \tss ->
        formatAnswer tss
        `shouldBe`
        Just (T.intercalate argDelimiter $ map processArg tss)
    prop "escaped Text does not contain control sequences" $ \t ->
      not $ any
        (`T.isInfixOf` (content $ fromMaybe "" $ formatAnswer [[t]]))
        controlSequences

  describe "formatForJS" $ do
    it "does not change non unicode text and puts it in a printed list" $
      forAll (arbitrary `suchThat` T.all isAscii) $ \t ->
        formatForJS (fromJust $ formatAnswer [[t]]) `shouldBe` T.pack (show [emptyOrNone t])
    it "converts haskell unicode chars into JavaScript (\\u) for a unit test" $
      formatForJS (fromJust $ formatAnswer [[jsUnitTest]]) `shouldBe` "[\"\\u04d2\\u29b6\"]"

  describe "removeUnicodeEscape" $ do
    it "leaves ascii chars alone" $
      forAll (arbitrary `suchThat` all isAscii) $ \s ->
        removeUnicodeEscape s `shouldBe` s
    it "strips an escape char off of any unicode." $
      forAll (chooseInt (999,100000)) $ \i ->
        removeUnicodeEscape ('\\': show i) `shouldBe` show i

  where
    formatUnitTest = T.concat $ intersperse argDelimiter
      [ escaped "one"
      , escaped missingMarker
      , escaped emptyMarker
      , T.concat
        [ escaped "two"
        , listDelimiter
        , escaped "three"
        ]
      ]

    jsUnitTest = "\1234\10678"

    controlSequences =
      [ argDelimiter
      , listDelimiter
      , inputEscape <> inputEscape
      ]

    content = T.drop 2 . T.dropEnd 2


processArg :: [Text] -> Text
processArg [] = escaped missingMarker
processArg xs = T.intercalate listDelimiter $ map escaped xs


escaped :: Text -> Text
escaped t = inputEscape <> pack (show $ emptyOrNone t) <> inputEscape


emptyOrNone :: Text -> Text
emptyOrNone "" = emptyMarker
emptyOrNone x = x
