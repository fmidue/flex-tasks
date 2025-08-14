{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}

{- |
Generic Parsing interface for submission types.
-}

module FlexTask.Generic.Parse (
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
  ) where


import GHC.TypeLits (TypeError, ErrorMessage(Text,(:$$:)))

import FlexTask.Generic.ParseInternal


instance {-# Overlappable #-} TypeError ('Text "Parse instances for nested lists are not supported."
              ':$$: 'Text "Please use a newtype or custom datatype instead."
                   )
         => Parse [[a]] where
   formParser = error "unreachable"
