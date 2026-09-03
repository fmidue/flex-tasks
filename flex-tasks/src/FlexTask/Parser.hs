{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}

{- |
Main interface for parsing form submissions, including the standard input types.
-}

module FlexTask.Parser (
  Parse(..),
  -- * Parse Helpers
  parseInstanceSingleChoice,
  parseInstanceMultiChoice,
  parseInstanceSingleInputList,
  escaped,
  -- * Embedding Functions
  parseWithOrReport,
  reportWithFieldNumber,
  parseInfallibly,
  -- * Error Processing
  parseWithFallback,
  displayInputAnd,
  -- * Debugging
  asSubmission,
  -- * Re-export
  module FlexTask.InputTypes,
  ) where


import GHC.TypeLits (TypeError, ErrorMessage(Text,(:$$:)))

import FlexTask.InputTypes
import FlexTask.Parser.Internal


instance {-# Overlappable #-} TypeError (
  'Text "Parse instances for nested lists are not supported."
  ':$$: 'Text "Please use a newtype or custom datatype instead."
  ) => Parse [[a]] where
  formParser = error "unreachable"
