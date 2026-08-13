{-# language DataKinds #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Generic.Form (
  -- * Data Types
    Alignment(..)
  , ChoiceShape(..)
  , TypeField
  , Requiredness
  , CompleteForm
  , SimpleFormPiece
  , AnyFormPiece
  , FormLayout
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
    -- * Anonymous Enum Type Builders and Accessors.
  , getAnswer
  , getAnswerAsIndex
  , getAnswers
  , getAnswersAsIndices
  , multipleChoiceAnswer
  , multipleChoiceEmpty
  , singleChoiceAnswer
  , singleChoiceEmpty

    -- * Field Builders
  , basic
  , required
  , optional
  , (>|)
  , beside
  , (>-)
  , above
  , singleChoice
  , singleChoiceEnum
  , multipleChoice
  , multipleChoiceEnum
  , list
  , listWithoutLabels
  , repeatFieldInfo
  , repeatBuilderOn
  , single
  ) where

import FlexTask.Generic.FormInternal
