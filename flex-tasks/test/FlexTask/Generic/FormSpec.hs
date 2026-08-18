{-# OPTIONS_GHC -Wno-orphans #-}

{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}

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
import FlexTask.Generic.Form
import FlexTask.YesodConfig             (FlexForm)



data TestEnum = One | Two | Three
  deriving (Bounded, Enum, Eq, Formify, Generic)


spec :: Spec
spec = do
  describe "formify" $ do
    context "should work for all standard types" $ do
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


runTest :: Formify a => Gen (CompleteForm a) -> Property
runTest gen = forAll (Blind <$> gen) testWith
  where
    testWith (Blind fi) =
      getFormData (formify Nothing fi) `shouldNotThrow` anyErrorCall


instance Arbitrary Alignment where
  arbitrary = elements [Vertical,Horizontal]


instance Arbitrary ChoiceShape where
  arbitrary = do
    align <- arbitrary
    elements [Buttons align, Dropdown]


choiceForm
  :: ( ChoiceShape
    -> FieldSettings FlexForm
    -> [SomeMessage FlexForm]
    -> TypeField a
    )
  -> Gen (SimpleFormPiece a a)
choiceForm f = do
  shape <- arbitrary
  title <- arbitrary
  labels <- chooseInt (1,100) >>= flip vectorOf arbitrary
  pure $ single $ f shape title labels


singleChoiceForm :: Gen (CompleteForm SingleChoiceSelection)
singleChoiceForm = choiceForm singleChoice


multipleChoiceForm :: Gen (CompleteForm MultipleChoiceSelection)
multipleChoiceForm = choiceForm multipleChoice


choiceFormEnum
  :: (Bounded a, Enum a, Eq a)
  => ( ChoiceShape
    -> FieldSettings FlexForm
    -> (a -> SomeMessage FlexForm)
    -> TypeField b
    )
  -> Gen (SimpleFormPiece b b)
choiceFormEnum f = do
  shape <- arbitrary
  title <- arbitrary
  labels <- zip range <$> vectorOf (length range) arbitrary
  pure $ single $ f shape title $ toText labels
  where
    range = [minBound .. maxBound]
    toText mapping enum = fromMaybe (fromString "") $ lookup enum mapping


singleChoiceFormEnum :: (Bounded a, Enum a, Eq a) => Gen (SimpleFormPiece a a)
singleChoiceFormEnum = choiceFormEnum singleChoiceEnum


multipleChoiceFormEnum :: (Bounded a, Enum a, Eq a) => Gen (CompleteForm (MultipleChoice a))
multipleChoiceFormEnum = choiceFormEnum multipleChoiceEnum


listForm :: BaseField a => Gen (ListFormPiece t a)
listForm = do
  align <- arbitrary
  amount <- chooseInt (1,100)
  labels <- vectorOf amount arbitrary
  attributes <- chooseInt (1,20) >>= flip vectorOf arbitrary
  elements [list align basic labels,listWithoutLabels align amount basic attributes]


requiredListForm :: BaseField a => Gen (CompleteForm [a])
requiredListForm = listForm


optionalListForm :: BaseField a => Gen (CompleteForm [Maybe a])
optionalListForm = listForm


instance Arbitrary (SomeMessage FlexForm) where
  arbitrary = fromString <$> arbitrary


instance Arbitrary (FieldSettings FlexForm) where
  arbitrary = fromString <$> arbitrary


simpleForm :: BaseField a => Gen (SimpleFormPiece a a)
simpleForm = single . basic <$> arbitrary


optionalSimpleForm :: BaseField a => Gen (CompleteForm (Maybe a))
optionalSimpleForm = single . basic <$> arbitrary
