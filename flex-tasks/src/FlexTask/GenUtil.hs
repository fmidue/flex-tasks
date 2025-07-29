
module FlexTask.GenUtil (
  fromGen,
  ) where


import Control.Monad.Random             (MonadRandom(getRandom))
import Test.QuickCheck.Gen              (Gen(unGen))
import Test.QuickCheck.Random           (mkQCGen)



{- |
Convert a QuickCheck generator into a member of MonadRandom.
-}
fromGen :: MonadRandom m => Gen a -> m a
fromGen gen = do
  seed <- getRandom
  pure $ unGen gen (mkQCGen seed) 30
