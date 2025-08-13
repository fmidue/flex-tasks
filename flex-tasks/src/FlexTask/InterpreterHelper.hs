{-# OPTIONS_GHC -Wno-orphans #-}
{-# language ApplicativeDo #-}
module FlexTask.InterpreterHelper (syntaxAndSemantics) where


import Control.Monad.Catch              (MonadCatch(..), MonadThrow(..))
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Random       (RandT, liftCatch)
import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Generic (($>>=))
import Control.OutputCapable.Blocks.Type (
  Output,
  getOutputSequenceWithResult,
  getOutputSequenceWithRating,
  )



instance MonadThrow (RandT g IO) where
  throwM = lift . throwM


instance MonadCatch (RandT g IO) where
  catch = liftCatch catch


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
