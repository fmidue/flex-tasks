{-# OPTIONS_GHC -Wno-orphans #-}
module FlexTask.Interpreter.GenerationHelper (
  getFormData,
  ) where


import Capabilities.Alloy               (MonadAlloy(..))
import Capabilities.Alloy.IO            ()
import Capabilities.Diagrams            (MonadDiagrams(..))
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz            (MonadGraphviz(..))
import Capabilities.Graphviz.IO         ()
import Control.Monad.Catch              (MonadCatch(..), MonadThrow (..))
import Control.Monad.Trans.Random       (RandT, liftCatch)
import Control.Monad.Trans.Class        (lift)

import FlexTask.ConvertForm              (getFormData)



instance MonadThrow (RandT g IO) where
  throwM = lift . throwM

instance MonadCatch (RandT g IO) where
  catch = liftCatch catch

instance MonadAlloy (RandT g IO) where
  getInstancesWith config = lift . getInstancesWith config

instance MonadDiagrams (RandT g IO) where
  lin = lift lin
  renderDiagram = lift . renderDiagram

instance MonadGraphviz (RandT g IO)  where
  errorWithoutGraphviz = lift errorWithoutGraphviz
  layoutGraph command = lift . layoutGraph command
  layoutGraph' params command = lift . layoutGraph' params command
