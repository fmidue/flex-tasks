{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}
module FlexTask.Generic.Parse (
  module FlexTask.Generic.ParseInternal
  ) where


import GHC.TypeLits (TypeError, ErrorMessage(Text,(:$$:)))

import FlexTask.Generic.ParseInternal

instance TypeError ('Text "Parse instances for nested lists are not supported."
              ':$$: 'Text "Please use a newtype or custom datatype instead."
                   )
         => Parse [[a]] where
   parseInput = error "unreachable"

