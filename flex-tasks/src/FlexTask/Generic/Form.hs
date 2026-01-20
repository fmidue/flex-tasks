{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Generic.Form (
  -- * Data Types
    Alignment(..)
  , FieldInfo
  , SingleChoiceSelection
  , MultipleChoiceSelection
  , Hidden(..)
  , SingleInputList(..)
    -- * Type Classes
  , BaseForm(..)
  , Formify(..)
  , formify
  , formifyComponents
  , formifyComponentsFlat
    -- * Anonymous Enum Type Builders and Accessors.
  , getAnswer
  , getAnswers
  , multipleChoiceAnswer
  , multipleChoiceEmpty
  , singleChoiceAnswer
  , singleChoiceEmpty

    -- * Field Builders
  , buttons
  , buttonsEnum
  , dropdown
  , dropdownEnum
  , list
  , listWithoutLabels
  , repeatFieldInfo
  , repeatBuilderOn
  , single

    -- * Formify Convenience Functions
  , formifyInstanceBasicField
  , formifyInstanceOptionalField
  , formifyInstanceSingleChoice
  , formifyInstanceMultiChoice
  ) where


import GHC.TypeLits (TypeError, ErrorMessage(Text,(:$$:)))

import FlexTask.Generic.FormInternal


instance {-# Overlappable #-} TypeError (
  'Text "Formify instances for nested lists are not supported."
  ':$$: 'Text "Please use a newtype or custom datatype instead."
  ) => Formify [[a]] where
  formifyImplementation = error "unreachable"

