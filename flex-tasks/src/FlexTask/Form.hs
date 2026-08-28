{-# language ExplicitNamespaces #-}

{- |
Typed construction and rendering of `Yesod` input forms.

Forms are built from type-safe `FormPiece`s describing individual fields and their layout.
Pieces can be combined horizontally or vertically and rendered as a `CompleteForm` using `formify`.

The result type of a form determines the types and requiredness of its fields.
For custom product types, this structure can be derived generically through `Formify`.

This module provides basic input fields, single- and multiple-choice fields,
list forms, and several specialized field types.
-}

module FlexTask.Form (
  -- * Form Types
    Formify(FormTypes)
  , FormPiece
  , SimpleFormPiece
  , ListFormPiece
  , AnyFormPiece
  , CompleteForm
  -- * Creating Form Pieces
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
  -- * Composition and Layout
  , (>|)
  , beside
  , (>-)
  , above
  -- * Rendering
  , formify
  , formifyComponents
  , formifyComponentsFlat
  -- * Appearance and Layout Options
  , Alignment(..)
  , ChoiceShape(..)
  -- * Specialized Field Types
  , Hidden(..)
  , SingleInputList(..)
  , MultipleChoice(..)
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
  -- * Type-Level Helper Types
  , OneField
  , ManyFields
  , (:>)
  , type (++)
  , CanBeOptional
  ) where


import FlexTask.InputTypes
import FlexTask.Form.Formify
import FlexTask.Form.Internal
import FlexTask.Form.TypeLevel


{- $TypeFields

The functions in this section behave identically to their counterparts producing a `FormPiece`,
for example `basicField` behaves like `basic`, but produces a `TypeField` instead.

Their purpose is to restrict the options that can be used with `list` at the type level.
The builder passed to `list` must produce a `TypeField`.
It cannot produce a composed `FormPiece`, such as one obtained using `>|`.

The restriction is necessary,
because the current approach to parsing the user input cannot handle arbitrary combinations,
but only a specific subset of those.
There is also no `TypeField` equivalent for `multipleChoice` and `multipleChoiceEnum` for the same reason.
Lists of `MultipleChoice` forms are therefore not supported at the moment.
-}
