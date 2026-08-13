{-# language DataKinds #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Generic.Form (
  -- * Data Types
  -- ** Field Construction
    TypeField
  , Requiredness
  -- ** Layouting
  , FormLayout
  , SimpleFormPiece
  , AnyFormPiece
  , CompleteForm
  -- ** General Purpose
  , Alignment(..)
  , ChoiceShape(..)
  , SingleChoiceSelection
  , MultipleChoiceSelection
  , Hidden(..)
  , SingleInputList(..)
  -- * Type Classes
  , BaseForm(..)
  , Formify
  -- * Generating Forms
  , formify
  , formifyComponents
  , formifyComponentsFlat
  -- * Functions for SingleChoiceSelection/MultipleChoiceSelection
  , getAnswer
  , getAnswerAsIndex
  , getAnswers
  , getAnswersAsIndices
  , multipleChoiceAnswer
  , multipleChoiceEmpty
  , singleChoiceAnswer
  , singleChoiceEmpty
  -- * Builders
  -- ** Field Construction
  , basic
  , singleChoice
  , singleChoiceEnum
  , multipleChoice
  , multipleChoiceEnum
  , required
  , optional
  -- ** Layouting
  , single
  , (>|)
  , beside
  , (>-)
  , above
  , list
  , listWithoutLabels
  , repeatFieldInfo
  , repeatBuilderOn
  ) where

import FlexTask.Generic.FormInternal
