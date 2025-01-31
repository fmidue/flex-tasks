{-# language ApplicativeDo #-}
module FlexTask.InterpreterHelper (syntaxAndSemantics) where


import Control.OutputCapable.Blocks     (LangM, LangM', Rated, ReportT)
import Control.OutputCapable.Blocks.Type
import Control.OutputCapable.Blocks.Generic (
  ($>>=),
  )
import Data.Either                      (fromRight)



type Report = ReportT Output IO


syntaxAndSemantics
  :: (String -> Either (LangM Report) (LangM' Report b))
  -> (a -> FilePath -> b -> LangM Report)
  -> (a -> FilePath -> b -> Rated Report)
  -> String
  -> a
  -> FilePath
  -> IO ([Output], Maybe (Maybe Rational, [Output]))
syntaxAndSemantics preprocess syntax semantics input tData path  = do
  let
    parserRes = preprocess input
    syn = either id ($>>= syntax tData path) parserRes
  synRes <- getOutputSequence syn
  if any isAbort synRes
    then
      pure (synRes,Nothing)
    else do
      let sem = fromRight undefined parserRes $>>= semantics tData path
      semRes <- getOutputSequenceWithRating sem
      pure (synRes, Just semRes)


isAbort :: Output -> Bool
isAbort (Refuse _)          = True
isAbort (Assertion False _) = True
isAbort _                   = False
