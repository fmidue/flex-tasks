{-# language ApplicativeDo #-}
module FlexTask.InterpreterHelper (syntaxAndSemantics) where


import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Type (
  Output,
  getOutputSequenceAndResult,
  getOutputSequenceWithRating,
  )



type Report = ReportT Output IO


syntaxAndSemantics
  :: (FilePath -> a -> b -> LangM Report)
  -> (FilePath -> a -> b -> Rated Report)
  -> LangM' Report b
  -> FilePath
  -> a
  -> IO ([Output], Maybe (Maybe Rational, [Output]))
syntaxAndSemantics syntax semantics preprocessed path tData = do
  (mParseResult,parseOutput) <- getOutputSequenceAndResult preprocessed
  case mParseResult of
    Nothing          -> pure (parseOutput,Nothing)
    Just parseResult -> do
      (synSuccess,synRes) <- getOutputSequenceAndResult $ syntax path tData parseResult
      let parseAndSyntax = parseOutput ++ synRes
      case synSuccess of
        Nothing -> pure (parseAndSyntax,Nothing)
        Just () -> do
          let sem = semantics path tData parseResult
          semRes <- getOutputSequenceWithRating sem
          pure (parseAndSyntax, Just semRes)
