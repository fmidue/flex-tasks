{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Form (
  -- * Data Types
    Alignment(..)
  , FieldInfo

    -- * Type Classes
  , BaseForm(..)
  , Formify(..)
  , formify
  , formifyComponents
  , formifyComponentsFlat

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
  , formifyInstanceOptionalSingleChoice
  , formifyInstanceMultiChoice
  ) where


import GHC.TypeLits (TypeError, ErrorMessage(Text,(:$$:)))

import FlexTask.Form.Internal


instance {-# Overlappable #-} TypeError (
  'Text "Formify instances for nested lists are not supported."
  ':$$: 'Text "Please use a newtype or custom datatype instead."
  ) => Formify [[a]] where
  formifyImplementation = error "unreachable"

