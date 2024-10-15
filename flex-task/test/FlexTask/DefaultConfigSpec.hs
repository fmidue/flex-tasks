{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module FlexTask.DefaultConfigSpec where


import Control.OutputCapable.Blocks     (ReportT)
import System.Environment               (setEnv)
import Test.Hspec (
  Spec,
  anyErrorCall,
  beforeAll,
  context,
  describe,
  it,
  runIO,
  )
import Test.Hspec.Parsec                (shouldParse)
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
import FlexTask.TestUtil                (shouldNotThrow)


type TestReport = ReportT (IO ()) IO


spec :: Spec
spec = do
  describe "defaultConfig" $ do
    it "is parsed by the config parser" $
      parse parseFlexConfig "" (showFlexConfig defaultConfig)
      `shouldParse` defaultConfig
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
