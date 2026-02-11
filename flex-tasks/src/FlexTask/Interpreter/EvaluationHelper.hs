{-# language ApplicativeDo #-}
module FlexTask.Interpreter.EvaluationHelper (
  syntaxAndSemantics,
  ) where


import Control.Monad.Identity           (Identity, runIdentity)
import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Generic (
  ($>>=),
  )
import Control.OutputCapable.Blocks.Type (
  Output,
  getOutputSequenceWithResult,
  getOutputSequenceWithRating,
  )



syntaxAndSemantics
  :: (String -> LangM' (ReportT Output Identity) b)
  -> (FilePath -> a -> b -> LangM (ReportT Output Identity))
  -> (FilePath -> a -> b -> Rated (ReportT Output IO))
  -> String
  -> FilePath
  -> a
  -> IO ([Output], Maybe (Maybe Rational, [Output]))
syntaxAndSemantics preprocess syntax semantics input path tData = do
  let syn = syntax path tData
  let (syntaxResult,syntaxOutput) = runIdentity $ getOutputSequenceWithResult $
        preprocess input $>>= \res -> res <$ syn res
  (syntaxOutput,) <$> case syntaxResult of
    Nothing -> pure Nothing
    Just a -> do
      let sem = semantics path tData
      semanticsResult <- getOutputSequenceWithRating $ sem a
      pure (Just semanticsResult)
