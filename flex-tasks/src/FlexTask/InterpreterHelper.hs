{-# language ApplicativeDo #-}
module FlexTask.InterpreterHelper (syntaxAndSemantics) where


import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Generic (($>>=))
import Control.OutputCapable.Blocks.Type (
  Output,
  getOutputSequenceWithResult,
  getOutputSequenceWithRating,
  )



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
