{-# OPTIONS_GHC -Wno-orphans #-}

{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TypeOperators #-}

module FlexTask.Generic.FormSpec where


import Data.Maybe                       (fromMaybe)
import Data.String                      (fromString)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)
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
  Blind(..),
  Gen,
  Property,
  chooseInt,
  elements,
  forAll,
  vectorOf,
  )
import Test.QuickCheck.Instances.Text   ()
import Yesod                            (FieldSettings, SomeMessage, Textarea)

import FlexTask.TestUtil                (shouldNotThrow)
import FlexTask.ConvertForm             (getFormData)
import FlexTask.Generic.FormGADT
import FlexTask.YesodConfig             (FlexForm)



data TestEnum = One | Two | Three
  deriving (Bounded, Enum, Eq, Formify, Generic)


spec :: Spec
spec = do
  describe "formify" $ do
    context "should work for all standard types" $ do
      specify "String" $
        runTest @String simpleForm
      specify "Text" $
        runTest @Text simpleForm
      specify "TextArea" $
        runTest @Textarea simpleForm
      specify "Bool" $
        runTest @Bool simpleForm
      specify "Int" $
        runTest @Int simpleForm
      specify "Double" $
        runTest @Double simpleForm

    context "should work for optional values" $ do
      specify "String" $
        runTest $ optionalSimpleForm @String
      specify "Text" $
        runTest $ optionalSimpleForm @Text
      specify "Textarea" $
        runTest $ optionalSimpleForm @Textarea
      specify "Bool" $
        runTest $ optionalSimpleForm @Bool
      specify "Int" $
        runTest $ optionalSimpleForm @Int
      specify "Double" $
        runTest $ optionalSimpleForm @Double

    context "should work for lists" $ do
      specify "String" $
        runTest $ requiredListForm @String
      specify "Text" $
        runTest $ requiredListForm @Text
      specify "Textarea" $
        runTest $ requiredListForm @Textarea
      specify "Bool" $
        runTest $ requiredListForm @Bool
      specify "Int" $
        runTest $ requiredListForm @Int
      specify "Double" $
        runTest $ requiredListForm @Double

    context "should work for lists of optional values" $ do
      specify "String" $
        runTest $ optionalListForm @String
      specify "Text" $
        runTest $ optionalListForm @Text
      specify "Textarea" $
        runTest $ optionalListForm @Textarea
      specify "Bool" $
        runTest $ optionalListForm @Bool
      specify "Int" $
        runTest $ optionalListForm @Int
      specify "Double" $
        runTest $ optionalListForm @Double

    describe "Anonymous Enums" $ do
      it "single choice works" $
        runTest singleChoiceForm
      it "multiple choice works" $
        runTest multipleChoiceForm

    describe "custom enum functions (for a single test type)" $ do
      it "single choice works" $
        runTest @TestEnum singleChoiceFormEnum
      it "multiple choice works" $
        runTest $ multipleChoiceFormEnum @TestEnum


runTest :: Formify a => Gen (FormSpec a) -> Property
runTest gen = forAll (Blind <$> gen) testWith
  where
    testWith (Blind fi) =
      getFormData (formify Nothing fi) `shouldNotThrow` anyErrorCall


instance Arbitrary Alignment where
  arbitrary = elements [Vertical,Horizontal]


choiceForm
  :: FormTypes a ~ '[a]
  => ( Alignment
    -> FieldSettings FlexForm
    -> [SomeMessage FlexForm]
    -> FieldInfo a
    )
  -> ( FieldSettings FlexForm
    -> [SomeMessage FlexForm]
    -> FieldInfo a
    )
  -> Gen (FormSpec a)
choiceForm f g = do
  align <- arbitrary
  title <- arbitrary
  labels <- chooseInt (1,100) >>= flip vectorOf arbitrary
  single . required <$> elements [f align title labels, g title labels]


singleChoiceForm :: Gen (FormSpec SingleChoiceSelection)
singleChoiceForm = choiceForm buttons dropdown


multipleChoiceForm :: Gen (FormSpec MultipleChoiceSelection)
multipleChoiceForm = choiceForm multiButtons multiDropdown


choiceFormEnum
  :: (Bounded a, Enum a, Eq a, FormTypes b ~ '[b])
  => ( Alignment
    -> FieldSettings FlexForm
    -> (a -> SomeMessage FlexForm)
    -> FieldInfo b
    )
  -> ( FieldSettings FlexForm
    -> (a -> SomeMessage FlexForm)
    -> FieldInfo b
    )
  -> Gen (FormSpec b)
choiceFormEnum f g = do
  align <- arbitrary
  title <- arbitrary
  labels <- zip range <$> vectorOf (length range) arbitrary
  single . required <$> elements [
    f align title $ toText labels,
    g title $ toText labels
    ]
  where
    range = [minBound .. maxBound]
    toText mapping enum = fromMaybe (fromString "") $ lookup enum mapping


singleChoiceFormEnum :: (Bounded a, Enum a, Eq a, FormTypes a ~ '[a]) => Gen (FormSpec a)
singleChoiceFormEnum = choiceFormEnum buttonsEnum dropdownEnum


multipleChoiceFormEnum :: (Bounded a, Enum a, Eq a) => Gen (FormSpec [a])
multipleChoiceFormEnum = choiceFormEnum multiButtonsEnum multiDropdownEnum


listForm :: BaseForm a => (FieldInfo a -> Requiredness b) -> Gen (FormSpec [b])
listForm req = do
  align <- arbitrary
  amount <- chooseInt (1,100)
  labels <- vectorOf amount arbitrary
  attributes <- chooseInt (1,20) >>= flip vectorOf arbitrary
  elements [list align (req . basic) labels,listWithoutLabels align amount (req . basic) attributes]


requiredListForm :: BaseForm a => Gen (FormSpec [a])
requiredListForm = listForm required


optionalListForm :: BaseForm a => Gen (FormSpec [Maybe a])
optionalListForm = listForm optional


instance Arbitrary (SomeMessage FlexForm) where
  arbitrary = fromString <$> arbitrary


instance Arbitrary (FieldSettings FlexForm) where
  arbitrary = fromString <$> arbitrary


simpleForm :: (BaseForm a, FormTypes a ~ '[a]) => Gen (FormSpec a)
simpleForm = single . required . basic <$> arbitrary


optionalSimpleForm :: BaseForm a => Gen (FormSpec (Maybe a))
optionalSimpleForm = single . optional . basic <$> arbitrary
