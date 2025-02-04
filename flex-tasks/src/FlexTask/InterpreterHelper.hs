{-# language ApplicativeDo #-}
module FlexTask.InterpreterHelper (syntaxAndSemantics) where


import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Type
import Control.OutputCapable.Blocks.Generic (
  ($>>=),
  )
import Data.Functor                     (($>))



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
  quitOnAbort (parserRes $> ()) $ const $ do
    quitOnAbort (parserRes $>>= syntax tData path) $ \synRes -> do
      let sem = parserRes $>>= semantics tData path
      semRes <- getOutputSequenceWithRating sem
      pure (synRes, Just semRes)
  where
    parserRes = preprocess input
    quitOnAbort theThing continueWith = do
      output <- getOutputSequence theThing
      if hasAbort output
        then pure (output,Nothing)
        else continueWith output


hasAbort :: [Output] -> Bool
hasAbort = any $ withRefusal $ const False
