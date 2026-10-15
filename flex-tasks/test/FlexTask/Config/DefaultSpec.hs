{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module FlexTask.Config.DefaultSpec where


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

import FlexTask.Config.Default
import FlexTask.Config.Types (
  CommonModules(..),
  FlexInstance(..),
  parseFlexConfig,
  showFlexConfig,
  validateFlexConfig,
  )
import FlexTask.Interpreter (
  checkSolution,
  genFlexInstance,
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
      beforeAll genInstance $ do
        it "and it can be used to build a description" $
          \FlexInstance {commonModules = CommonModules{..},..} ->
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
          \FlexInstance {commonModules = CommonModules{..},..} ->
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
    genInstance = genFlexInstance
      defaultConfig
      91275060
