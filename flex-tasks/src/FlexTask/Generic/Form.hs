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
  -- $TypeFields
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


{- $TypeFields

The functions in this section are identical to their counterparts producing a `FormPiece`,
for example `basicField` is the same as `basic`, but has a different type.

Their purpose is to restrict the options that can be used with `list` on the type level.
`list` must take a `TypeField` value to multiply.
It cannot take a composed `FormPiece` obtained through use of e.g. `>|`.

The restriction is necessary,
because the current approach to parsing the user input cannot handle arbitrary combinations,
but only a specific subset of those.
There is also no `TypeField` equivalent for `multipleChoice` and `multipleChoiceEnum` for the same reason.
Lists of `MultipleChoice` forms are therefore not supported at the moment.
-}
