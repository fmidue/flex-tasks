{-# OPTIONS_GHC -Wno-orphans #-}
{-# language AllowAmbiguousTypes #-}
{-# language OverloadedStrings #-}

module FlexTask.Generic.FormSpec where


import Data.Maybe                       (fromMaybe)
import Data.Text                        (Text)
import Test.Hspec (
  Spec,
  anyErrorCall,
  context,
  describe,
  it,
  specify,
  )
import Test.QuickCheck (
  Arbitrary(..),
  Gen,
  Property,
  chooseInt,
  elements,
  forAll,
  vectorOf,
  )
import Test.QuickCheck.Instances.Text   ()
import Yesod                            (Textarea)

import FlexTask.TestUtil                (shouldNotThrow)
import FlexTask.FormUtil                (getFormData)
import FlexTask.Generic.Form



data TestEnum = One | Two | Three deriving (Bounded, Enum, Eq)

instance Formify TestEnum where
  formifyImplementation = formifyInstanceSingleChoice

instance Formify [TestEnum] where
  formifyImplementation = formifyInstanceMultiChoice


spec :: Spec
spec = do
  describe "formify" $ do
    context "should work for all standard types" $ do
      specify "String" $
        runTest @String singleInfo
      specify "Text" $
        runTest @Text singleInfo
      specify "TextArea" $
        runTest @Textarea singleInfo
      specify "Bool" $
        runTest @Bool singleInfo
      specify "Int" $
        runTest @Int singleInfo
      specify "Double" $
        runTest @Double singleInfo

    context "should work for optional values" $ do
      specify "String" $
        runTest @(Maybe String) singleInfo
      specify "Text" $
        runTest @(Maybe Text) singleInfo
      specify "Textarea" $
        runTest @(Maybe Textarea) singleInfo
      specify "Bool" $
        runTest @(Maybe Bool) singleInfo
      specify "Int" $
        runTest @(Maybe Int) singleInfo
      specify "Double" $
        runTest @(Maybe Double) singleInfo

    context "should work for lists" $ do
      specify "String" $
        runTest @[String] listInfo
      specify "Text" $
        runTest @[Text] listInfo
      specify "Textarea" $
        runTest @[Textarea] listInfo
      specify "Bool" $
        runTest @[Bool] listInfo
      specify "Int" $
        runTest @[Int] listInfo
      specify "Double" $
        runTest @[Double] listInfo

    context "should work for lists of optional values" $ do
      specify "String" $
        runTest @[Maybe String] listInfo
      specify "Text" $
        runTest @[Maybe Text] listInfo
      specify "Textarea" $
        runTest @[Maybe Textarea] listInfo
      specify "Bool" $
        runTest @[Maybe Bool] listInfo
      specify "Int" $
        runTest @[Maybe Int] listInfo
      specify "Double" $
        runTest @[Maybe Double] listInfo

    describe "Anonymous Enums" $ do
      it "single choice works" $
        runTest @SingleChoiceSelection choiceInfo
      it "multiple choice works" $
        runTest @MultipleChoiceSelection choiceInfo

    describe "custom enum functions (for a single test type)" $ do
      it "single choice works" $
        runTest @TestEnum (choiceInfoEnum @TestEnum)
      it "multiple choice works" $
        runTest @[TestEnum] (choiceInfoEnum @TestEnum)


runTest :: forall a . Formify a => Gen [[FieldInfo]] -> Property
runTest gen = forAll gen testWith
  where
    testWith fi =
      getFormData (formify @a Nothing fi) `shouldNotThrow` anyErrorCall


instance Arbitrary Alignment where
  arbitrary = elements [Vertical,Horizontal]


choiceInfo :: Gen [[FieldInfo]]
choiceInfo = doubleNest $ do
  align <- arbitrary
  title <- arbitrary
  labels <- chooseInt (1,100) >>= flip vectorOf arbitrary
  elements [buttons align title labels, dropdown title labels]


choiceInfoEnum :: forall a . (Bounded a, Enum a, Eq a) => Gen [[FieldInfo]]
choiceInfoEnum = doubleNest $ do
  align <- arbitrary
  title <- arbitrary
  labels <- zip range <$> vectorOf (length range) arbitrary
  elements [
    buttonsEnum align title $ toText labels,
    dropdownEnum title $ toText labels
    ]
  where
    range = [minBound .. maxBound @a]
    toText mapping enum = fromMaybe "" $ lookup enum mapping


listInfo :: Gen [[FieldInfo]]
listInfo = doubleNest $ do
  align <- arbitrary
  amount <- chooseInt (1,100)
  labels <- vectorOf amount arbitrary
  elements [list align labels,listWithoutLabels align amount]


singleInfo :: Gen [[FieldInfo]]
singleInfo = doubleNest $ single <$> arbitrary


doubleNest :: Gen a -> Gen [[a]]
doubleNest = fmap $ (:[]) . (:[])
