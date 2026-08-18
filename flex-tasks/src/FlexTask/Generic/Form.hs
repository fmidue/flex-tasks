{-# language DataKinds #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Generic.Form (
  -- * Individual Field Construction
    BaseField(..)
  , TypeField
  , basic
  , singleChoice
  , singleChoiceEnum
  , multipleChoice
  , multipleChoiceEnum
  -- * Composition and Layouting
  , FormPiece
  , (>|)
  , beside
  , (>-)
  , above
  , SimpleFormPiece
  , ListFormPiece
  , AnyFormPiece
  , single
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
  ) where

import FlexTask.Generic.FormInternal
