{-# language OverloadedStrings #-}

module FlexTask.Processing.TextSpec where


import Data.Char                        (isAscii)
import Data.List                        (intersperse)
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Text                        (Text, isInfixOf, pack)
import Test.Hspec                       (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck            (modifyMaxSize, prop)
import Test.QuickCheck (
  Gen,
  NonEmptyList(..),
  arbitrary,
  chooseInt,
  forAll,
  suchThat,
  )
import Test.QuickCheck.Instances.Text   ()

import FlexTask.Processing.Text

import qualified Data.Text              as T



spec :: Spec
spec = do
  describe "formatAnswer" $ do
    it "returns Nothing if there is no input" $
      forAll genEmpty $ \tss ->
        formatAnswer tss `shouldBe` Nothing
    it "correctly encodes a simple unit test" $
      formatAnswer [[["one"]],[],[[""]],[["two","three"]]]
      `shouldBe`
      Just formatUnitTest
    modifyMaxSize (const 40) $
      prop "inserts delimiters and marks input correctly" $ \(NonEmpty nEs) ->
        let tss = map (map getNonEmpty . getNonEmpty) nEs in
          formatAnswer tss
          `shouldBe`
          Just (T.intercalate argDelimiter $ map (processArg . concat) tss)
    prop "escaped Text does not contain control sequences" $ \t ->
      not $ any
        (`T.isInfixOf` (content $ fromMaybe "" $ formatAnswer [[[t]]]))
        controlSequences

  describe "formatForJS" $ do
    it "does not change non unicode text and puts it in a printed list" $
      forAll (arbitrary `suchThat` noUnicode) $ \t ->
        formatForJS (fromJust $ formatAnswer [[[t]]]) `shouldBe` T.pack (show [emptyOrNone t])
    it "converts haskell unicode chars into JavaScript (\\u) for a unit test" $
      formatForJS (fromJust $ formatAnswer [[[jsUnitTest]]]) `shouldBe` "[\"\\u04d2\\u29b6\"]"

  describe "removeUnicodeEscape" $ do
    it "leaves ascii chars alone" $
      forAll (('\\' :) <$> genTestString 0 127) $ \i ->
        removeUnicodeEscape i `shouldBe` i
    it "strips an escape char off of any unicode." $
      forAll (genTestString 128 1114111) $ \i ->
        removeUnicodeEscape ('\\': i) `shouldBe` i

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
    noUnicode t = T.all isAscii t && not ("\\u" `isInfixOf` t)

    genTestString upper lower = show <$> chooseInt (upper,lower)


genEmpty :: Gen [[[Text]]]
genEmpty = do
  amount <- chooseInt (0,10000)
  pure $ replicate amount [[]]


processArg :: [Text] -> Text
processArg [] = escaped missingMarker
processArg xs = T.intercalate listDelimiter $ map escaped xs


escaped :: Text -> Text
escaped t = inputEscape <> pack (show $ emptyOrNone t) <> inputEscape


emptyOrNone :: Text -> Text
emptyOrNone "" = emptyMarker
emptyOrNone x = x
