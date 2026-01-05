{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module FlexTask.DefaultConfigSpec where


import Control.OutputCapable.Blocks     (Language(..), ReportT)
import Control.OutputCapable.Blocks.Debug (
  checkConfigWith,
  )

import Test.Hspec (
  Spec,
  anyErrorCall,
  beforeAll,
  context,
  describe,
  it,
  shouldReturn,
  )
import Test.Hspec.Parsec                (shouldParse)
import Text.Parsec                      (parse)

import FlexTask.DefaultConfig
import FlexTask.Types (
  CommonModules(..),
  FlexInst(..),
  parseFlexConfig,
  showFlexConfig,
  validateFlexConfig,
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
    it "passes the config validation" $
      checkConfigWith English defaultConfig validateFlexConfig `shouldReturn` True
    context "generates an instance without throwing an error..." $ do
      beforeAll genInst $ do
        it "and it can be used to build a description" $
          \FlexInst{commonModules = CommonModules{..},..} ->
            validDescription @TestReport
              taskName
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
              taskName
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
      91275060
