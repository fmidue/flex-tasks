{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module FlexTask.DefaultConfigSpec where


import Control.OutputCapable.Blocks     (ReportT)
import Test.Hspec (
  Spec,
  anyErrorCall,
  beforeAll,
  context,
  describe,
  it,
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
import FlexTask.TestUtil (
  interpreterError,
  shouldNotThrow,
  shouldNotReturnLeft,
  )



type TestReport = ReportT (IO ()) IO


spec :: Spec
spec = do
  describe "defaultConfig" $ do
    it "is parsed by the config parser" $
      parse parseFlexConfig "" (showFlexConfig defaultConfig)
      `shouldParse` defaultConfig
    context "generates an instance without throwing an error..." $ do
      beforeAll genInst $ do
        it "and it can be used to build a description" $
          \FlexInst{commonModules = CommonModules{..},..} ->
            validDescription @TestReport
              taskData
              globalModule
              settingsModule
              descriptionModule
              extraModules
              ""
            `shouldNotThrow` anyErrorCall
        it "and it can be used to validate a submission" $
          \FlexInst{commonModules = CommonModules{..},..} ->
            checkSolution
              taskData
              globalModule
              settingsModule
              parseModule
              checkModule
              extraModules
              "test"
              ""
            `shouldNotReturnLeft` interpreterError
  where
    genInst = genFlexInst
      defaultConfig
      (\gen seed -> unGen gen (mkQCGen (read seed)) 30)
      "91275060"
