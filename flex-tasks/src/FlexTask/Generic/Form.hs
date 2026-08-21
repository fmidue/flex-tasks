{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# options_ghc -Wno-orphans #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Generic.Form (
  -- * Composition and Layouting
    FormPiece
  , (>|)
  , beside
  , (>-)
  , above
  -- * Typed Forms
  , SimpleFormPiece
  , ListFormPiece
  , AnyFormPiece
  , BaseField(..)
  , basic
  , singleChoice
  , singleChoiceEnum
  , multipleChoice
  , multipleChoiceEnum
  , list
  , listRepeatedly
  , listWithoutLabels
  -- * Fields for List Forms
  , TypeField
  , basicField
  , singleChoiceField
  , singleChoiceEnumField
  -- * Rendering Completed Forms
  , Formify(FormTypes)
  , CompleteForm
  , formify
  , formifyComponents
  , formifyComponentsFlat
  -- * Selectors for Appearance and Layout
  , Alignment(..)
  , ChoiceShape(..)
  , MultipleChoice(..)
  -- * Types for Specialized Fields
  , Hidden(..)
  , SingleInputList(..)
  , SingleChoiceSelection
  , singleChoiceAnswer
  , singleChoiceEmpty
  , getAnswer
  , getAnswerAsIndex
  , MultipleChoiceSelection
  , multipleChoiceAnswer
  , multipleChoiceEmpty
  , getAnswers
  , getAnswersAsIndices
  -- * Helper Types
  , OneField
  , ManyFields
  , (:>)
  , type (++)
  ) where

import FlexTask.Generic.FormInternal
