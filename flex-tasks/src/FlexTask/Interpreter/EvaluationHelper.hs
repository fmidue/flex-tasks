{-# language ApplicativeDo #-}
module FlexTask.Interpreter.EvaluationHelper (
  syntaxAndSemantics,
  ) where


import Control.Monad.Identity           (Identity, runIdentity)
import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Generic (($>>=))
import Control.OutputCapable.Blocks.Type (
  Output,
  getOutputSequenceWithResult,
  getOutputSequenceWithRating,
  )



syntaxAndSemantics
  :: (String -> LangM' (ReportT Output Identity) b)
  -> (a -> b -> LangM (ReportT Output Identity))
  -> (FilePath -> a -> b -> Rated (ReportT Output IO))
  -> String
  -> FilePath
  -> a
  -> IO ([Output], Maybe (Maybe Rational, [Output]))
syntaxAndSemantics preprocess syntax semantics input path tData =
  let (maybeSyntaxResult, syntaxOutput) = runIdentity $ getOutputSequenceWithResult $
        preprocess input $>>= \result -> result <$ syntax tData result
  in (syntaxOutput,) <$> mapM
                          (getOutputSequenceWithRating . semantics path tData)
                          maybeSyntaxResult
