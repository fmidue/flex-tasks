{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module FlexTask.DefaultConfigSpec where


import Control.Exception                (Exception, try)
import Control.Monad                    (when)
import Control.OutputCapable.Blocks     (ReportT)
import Data.Typeable                    (typeOf)
import System.Environment               (setEnv)
import Test.Hspec (
  Selector,
  Spec,
  anyErrorCall,
  beforeAll,
  context,
  expectationFailure,
  describe,
  it,
  shouldBe,
  runIO,
  )
import Test.QuickCheck.Gen              (unGen)
import Test.QuickCheck.Random           (mkQCGen)
import Text.Parsec                      (parse)

import FlexTask.DefaultConfig
import FlexTask.Types (
  CommonModules(..),
  FlexInst(..),
  parseFlexConfig,
  showFlexConfig,
  )
import FlexTask.Interpreter (
  checkSolution,
  genFlexInst,
  validDescription,
  )



type TestReport = ReportT (IO ()) IO


spec :: Spec
spec = do
  describe "defaultConfig" $ do
    it "is parsed by the config parser" $
      parse parseFlexConfig "" (showFlexConfig defaultConfig)
      `shouldBe` Right defaultConfig
    _ <- runIO $ setEnv "FLEX_PKGDB" "none"
    context "generates an instance without throwing an error..." $ do
      beforeAll genInst $ do
        it "and it can be used to build a description" $
          \FlexInst{commonModules = CommonModules{..},..} ->
            validDescription @TestReport
              taskData
              globalModule
              descriptionModule
              ""
            `shouldNotThrow` anyErrorCall
        it "and it can be used to validate a submission" $
          \FlexInst{commonModules = CommonModules{..},..} ->
            checkSolution
              taskData
              globalModule
              parseModule
              checkModule
              "test"
              ""
            `shouldNotThrow` anyErrorCall
  where
    genInst = genFlexInst
      defaultConfig
      (\gen seed -> unGen gen (mkQCGen (read seed)) 30)
      "91275060"


shouldNotThrow :: Exception e => IO a -> Selector e -> IO ()
action `shouldNotThrow` p = do
  r <- try action
  case r of
    Right _ ->
      return ()
    Left e ->
      when (p e) $ expectationFailure $
        "predicate failed on " ++ exceptionType ++ ":\n" ++ show e
  where
    exceptionType = (show . typeOf . instanceOf) p

    instanceOf :: Selector a -> a
    instanceOf _ = error "broken Typeable instance"
