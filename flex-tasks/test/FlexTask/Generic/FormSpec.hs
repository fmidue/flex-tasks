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
import FlexTask.Generic.Form
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
  pure $ single $ required $ f shape title labels


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
  pure $ single $ required $ f shape title $ toText labels
  where
    range = [minBound .. maxBound]
    toText mapping enum = fromMaybe (fromString "") $ lookup enum mapping


singleChoiceFormEnum :: (Bounded a, Enum a, Eq a) => Gen (SimpleFormPiece a a)
singleChoiceFormEnum = choiceFormEnum singleChoiceEnum


multipleChoiceFormEnum :: (Bounded a, Enum a, Eq a) => Gen (CompleteForm [a])
multipleChoiceFormEnum = choiceFormEnum multipleChoiceEnum


listForm :: BaseForm a => (TypeField a -> Requiredness b) -> Gen (CompleteForm [b])
listForm req = do
  align <- arbitrary
  amount <- chooseInt (1,100)
  labels <- vectorOf amount arbitrary
  attributes <- chooseInt (1,20) >>= flip vectorOf arbitrary
  elements [list align (req . basic) labels,listWithoutLabels align amount (req . basic) attributes]


requiredListForm :: BaseForm a => Gen (CompleteForm [a])
requiredListForm = listForm required


optionalListForm :: BaseForm a => Gen (CompleteForm [Maybe a])
optionalListForm = listForm optional


instance Arbitrary (SomeMessage FlexForm) where
  arbitrary = fromString <$> arbitrary


instance Arbitrary (FieldSettings FlexForm) where
  arbitrary = fromString <$> arbitrary


simpleForm :: (BaseForm a) => Gen (SimpleFormPiece a a)
simpleForm = single . required . basic <$> arbitrary


optionalSimpleForm :: BaseForm a => Gen (CompleteForm (Maybe a))
optionalSimpleForm = single . optional . basic <$> arbitrary
