{-# language ApplicativeDo #-}
module FlexTask.Interpreter.EvaluationHelper (
  syntaxAndSemantics,
  ) where


import Control.Monad.Identity           (Identity, runIdentity)
import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Type (
  Output,
  getOutputSequenceWithResult,
  getOutputSequenceWithRating,
  )



type Report a = ReportT Output a


syntaxAndSemantics
  :: (String -> LangM' (Report Identity) b)
  -> (FilePath -> a -> b -> LangM (Report IO))
  -> (FilePath -> a -> b -> Rated (Report IO))
  -> String
  -> FilePath
  -> a
  -> IO ([Output], Maybe (Maybe Rational, [Output]))
syntaxAndSemantics preprocess syntax semantics input path tData = do
  let (mParseRes, parseOutput) = runIdentity $
        getOutputSequenceWithResult $ preprocess input
  case mParseRes of
    Nothing -> pure (parseOutput, Nothing)
    Just parseRes -> do
      let syn = syntax path tData
      (synSuccess,synRes) <- getOutputSequenceWithResult $ syn parseRes
      let syntaxOutput = parseOutput ++ synRes
      case synSuccess of
        Nothing -> pure (syntaxOutput,Nothing)
        Just () -> do
          let sem = semantics path tData
          semRes <- getOutputSequenceWithRating $ sem parseRes
          pure (syntaxOutput, Just semRes)
