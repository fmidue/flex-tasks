
module FlexTask.InterpreterHelper (syntaxAndSemantics) where


import Control.OutputCapable.Blocks     (LangM, Rated, ReportT, code, refuse)
import Control.OutputCapable.Blocks.Type
import Data.Either                      (fromRight)
import Text.Parsec                      (ParseError, sourceColumn)
import Text.Parsec.Error (
  errorMessages,
  errorPos,
  showErrorMessages,
  )



type Report = ReportT Output IO


syntaxAndSemantics
  :: (String -> Either ParseError b)
  -> (a -> FilePath -> b -> LangM Report)
  -> (a -> FilePath -> b -> Rated Report)
  -> String
  -> a
  -> FilePath
  -> IO ([Output], Maybe (Maybe Rational, [Output]))
syntaxAndSemantics parser syntax semantics input tData path  = do
  let
    parsed = parser input
    syn = either
      (refuse . code . showWithFieldNumber input)
      (syntax tData path)
      parsed
  synRes <- getOutputSequence syn
  if any isAbort synRes
    then
      pure (synRes,Nothing)
    else do
      let sem = semantics tData path (fromRight undefined parsed)
      semRes <- getOutputSequenceWithRating sem
      pure (synRes, Just semRes)


showWithFieldNumber :: String -> ParseError -> String
showWithFieldNumber input e = "Error in input field " ++ fieldNum ++ ":" ++ errors
  where
    fieldNum = show $ length (filter (=='\a') consumed) `div` 2 + 1
    errors = showErrorMessages
      "or"
      "unknown parse error"
      "expecting"
      "unexpected"
      "end of input"
      $ errorMessages e
    consumed = take (sourceColumn $ errorPos e) input


isAbort :: Output -> Bool
isAbort (Refuse _)          = True
isAbort (Assertion False _) = True
isAbort _                   = False
