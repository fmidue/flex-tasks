{-# OPTIONS_GHC -Wno-orphans #-}
{-# language ApplicativeDo #-}
module FlexTask.InterpreterHelper (
  syntaxAndSemantics,
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
import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Generic (($>>=))
import Control.OutputCapable.Blocks.Type (
  Output,
  getOutputSequenceWithResult,
  getOutputSequenceWithRating,
  )

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


type Report = ReportT Output IO


syntaxAndSemantics
  :: (String -> LangM' Report b)
  -> (FilePath -> a -> b -> LangM Report)
  -> (FilePath -> a -> b -> Rated Report)
  -> String
  -> FilePath
  -> a
  -> IO ([Output], Maybe (Maybe Rational, [Output]))
syntaxAndSemantics preprocess syntax semantics input path tData = do
  let parseRes = preprocess input
  let syn = syntax path tData
  (synSuccess,synRes) <- getOutputSequenceWithResult (parseRes $>>= syn)
  case synSuccess of
    Nothing -> pure (synRes,Nothing)
    Just () -> do
      let sem = semantics path tData
      semRes <- getOutputSequenceWithRating (parseRes $>>= sem)
      pure (synRes, Just semRes)
