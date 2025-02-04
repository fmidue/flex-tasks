{-# language ApplicativeDo #-}
module FlexTask.InterpreterHelper (syntaxAndSemantics) where


import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Type (
  Output,
  getOutputSequence,
  getOutputSequenceAndResult,
  getOutputSequenceWithRating,
  withRefusal,
  )
import Data.Maybe                       (fromMaybe)



type Report = ReportT Output IO


syntaxAndSemantics
  :: (String -> LangM' Report b)
  -> (a -> FilePath -> b -> LangM Report)
  -> (a -> FilePath -> b -> Rated Report)
  -> String
  -> a
  -> FilePath
  -> IO ([Output], Maybe (Maybe Rational, [Output]))
syntaxAndSemantics preprocess syntax semantics input tData path  = do
  (mParseResult,parseOutput) <- getOutputSequenceAndResult $ preprocess input
  if hasAbort parseOutput
    then pure (parseOutput,Nothing)
    else do
      let parseResult = getResult mParseResult
      synRes <- getOutputSequence (syntax tData path parseResult)
      let parseAndSyntax = parseOutput ++ synRes
      if hasAbort synRes
        then pure (parseAndSyntax,Nothing)
        else do
          let sem = semantics tData path parseResult
          semRes <- getOutputSequenceWithRating sem
          pure (parseAndSyntax, Just semRes)
  where
    getResult = fromMaybe (error "Result must be Just if output didn't abort!")



hasAbort :: [Output] -> Bool
hasAbort = any $ withRefusal $ const False
