module FlexTask.TestUtil where


import Control.Exception                (Exception, try)
import Control.Monad                    (when)
import Data.Typeable                    (Typeable, typeOf)
import Language.Haskell.Interpreter     (InterpreterError)
import Test.Hspec (
  Selector,
  expectationFailure,
  shouldReturn,
  )



shouldNotThrow :: Exception e => IO a -> Selector e -> IO ()
action `shouldNotThrow` p = do
  r <- try action
  case r of
    Right _ ->
      return ()
    Left e -> rejectOnFail p e


shouldNotReturnLeft :: Exception e => IO (Either e a) -> Selector e -> IO ()
action `shouldNotReturnLeft` p = do
  r <- action
  case r of
    Right _ ->
      return ()
    Left e -> rejectOnFail p e


rejectOnFail :: (Show e,Typeable e, Exception e) => Selector e -> e -> IO ()
rejectOnFail p e = when (p e) $ expectationFailure $
  "predicate failed on " ++ exceptionType ++ ":\n" ++ show e
  where
    exceptionType = show $ typeOf $ instanceOf p

    instanceOf :: Selector a -> a
    instanceOf _ = error "broken Typeable instance"


shouldReturnSame :: (Show a, Eq a) => IO a -> IO a -> IO ()
shouldReturnSame a b = a >>= shouldReturn b


interpreterError :: Selector InterpreterError
interpreterError = const True
