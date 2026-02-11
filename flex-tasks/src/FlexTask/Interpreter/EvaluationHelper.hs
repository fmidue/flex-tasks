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



syntaxAndSemantics
  :: (String -> LangM' (ReportT Output Identity) b)
  -> (FilePath -> a -> b -> LangM (ReportT Output IO))
  -> (FilePath -> a -> b -> Rated (ReportT Output IO))
  -> String
  -> FilePath
  -> a
  -> IO ([Output], Maybe (Maybe Rational, [Output]))
syntaxAndSemantics preprocess syntax semantics input path tData = do
  let (mParseResult, parseOutput) = runIdentity $
        getOutputSequenceWithResult $ preprocess input
  case mParseResult of
    Nothing -> pure (parseOutput, Nothing)
    Just parseResult -> do
      let syn = syntax path tData
      (syntaxSuccess,syntaxOutput) <- getOutputSequenceWithResult $ syn parseResult
      let syntaxResult = parseOutput ++ syntaxOutput
      (syntaxResult,) <$>
       case syntaxSuccess of
        Nothing -> pure Nothing
        Just () -> do
          let sem = semantics path tData
          semanticsResult <- getOutputSequenceWithRating $ sem parseResult
          pure (Just semanticsResult)
