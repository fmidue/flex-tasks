{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Generic.Form (
  module FlexTask.Generic.FormInternal
  ) where


import GHC.TypeLits (TypeError, ErrorMessage(Text,(:$$:)))

import FlexTask.Generic.FormInternal


instance {-# Overlappable #-} TypeError (
  'Text "Formify instances for nested lists are not supported."
  ':$$: 'Text "Please use a newtype or custom datatype instead."
  ) => Formify [[a]] where
  formifyImplementation = error "unreachable"

