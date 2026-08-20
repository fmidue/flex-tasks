{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# options_ghc -Wno-orphans #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Generic.Form (
  -- * Individual Field Construction
    BaseField(..)
  , TypeField
  , basicField
  , singleChoiceField
  , singleChoiceEnumField
  -- * Composition and Layouting
  , FormPiece
  , (>|)
  , beside
  , (>-)
  , above
  , SimpleFormPiece
  , ListFormPiece
  , AnyFormPiece
  , basic
  , singleChoice
  , singleChoiceEnum
  , multipleChoice
  , multipleChoiceEnum
  , list
  , listRepeatedly
  , listWithoutLabels
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
