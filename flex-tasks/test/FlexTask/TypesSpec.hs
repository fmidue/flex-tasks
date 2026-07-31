{-# OPTIONS_GHC -Wno-orphans #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}

module FlexTask.TypesSpec where


import Data.Char                        (isAscii, isLetter, toUpper)
import Data.List                        (intercalate)
import Data.List.Extra                  (notNull)
import Test.Hspec                       (Spec, describe, shouldBe)
import Test.Hspec.Parsec                (shouldParse)
import Test.Hspec.QuickCheck            (prop)
import Test.QuickCheck (
  Gen,
  chooseInt,
  elements,
  forAll,
  suchThat,
  listOf,
  listOf1,
  vectorOf,
  )
import Test.QuickCheck.Arbitrary        (Arbitrary(..))
import Text.Parsec                      (parse)

import FlexTask.Types


spec :: Spec
spec = do
  describe "showFlexConfig" $
    prop "segments the modules with delimiters correctly" $
      \fConf@FlexConf{commonModules = CommonModules{..},..} ->
      showFlexConfig fConf `shouldBe` intercalate delimiter (
        [ concat $
            ["taskName: " ++ taskName ++ "\r\n" | notNull taskName] ++
            ["validation: " ++ show validation]
        , globalModule
        , settingsModule
        , taskDataModule
        , descriptionModule
        , parseModule
        ] ++ map snd extraModules)

  describe "parseFlexConfig" $ do
    prop "successfully parses core and extra modules when task name is present" $
      \flag ->
        forAll ((,,) <$> vectorOf 5 arbitrary <*> listOf genExtraModule <*> genTaskName) $ \(mods,eMods,tName) ->
          parse parseFlexConfig "" (intercalate delimiter $
            unlines ["taskName: " ++ tName, "validation: " ++ show flag] : mods ++ map snd eMods
            ) `shouldParse`
          conf flag (tName : mods) eMods
    prop "successfully parses core and extra modules when task name is absent" $
      \flag ->
        forAll ((,) <$> vectorOf 5 arbitrary <*> listOf genExtraModule) $ \(mods,eMods) ->
          parse parseFlexConfig "" (intercalate delimiter (["validation: " ++ show flag] ++ mods ++ map snd eMods)) `shouldParse`
          conf flag mods eMods

  describe "both" $ do
    prop "are inverse to each other (provided extra modules are valid)" $ \fConf ->
      parse parseFlexConfig "" (showFlexConfig fConf) `shouldParse` fConf

    where
      conf validation [taskName, globalModule, settingsModule, taskDataModule, descriptionModule, parseModule] extraModules =
        FlexConf {
          validation,
          taskDataModule,
          commonModules = CommonModules {
            taskName,
            globalModule,
            settingsModule,
            descriptionModule,
            parseModule,
            extraModules
          }
        }
      conf validation [globalModule, settingsModule, taskDataModule, descriptionModule, parseModule] extraModules =
        FlexConf {
          validation,
          taskDataModule,
          commonModules = CommonModules {
            taskName = "",
            globalModule,
            settingsModule,
            descriptionModule,
            parseModule,
            extraModules
          }
        }
      conf _ modules extraModules = error $
        "This case should not be reachable: " ++
        show modules ++ "\n" ++
        show extraModules


instance Arbitrary CommonModules where
  arbitrary = do
    taskName <- genTaskName
    globalModule <- arbitrary
    settingsModule <- arbitrary
    descriptionModule <- arbitrary
    parseModule <- arbitrary
    amount <- chooseInt (1,5)
    extraModules <- vectorOf amount genExtraModule
    pure (CommonModules {
      taskName,
      globalModule,
      settingsModule,
      descriptionModule,
      parseModule,
      extraModules
      })


genTaskName :: Gen String
genTaskName = listOf1 (letter `suchThat` isAscii)


genExtraModule :: Gen (String,String)
genExtraModule = do
  firstChar <- toUpper <$> letter
  rest <- listOf letter
  let modName = firstChar : rest
  contents <- arbitrary
  pure (modName, "module " ++ modName ++ " where\n" ++ contents)


letter :: Gen Char
letter = arbitrary `suchThat` isLetter


instance Arbitrary ValidationFlag where
  arbitrary = elements [Validate, AssumeValid]

instance Arbitrary FlexConf where
  arbitrary = do
    validation <- arbitrary
    taskDataModule <- arbitrary
    commonModules <- arbitrary
    pure $ FlexConf {validation, taskDataModule, commonModules}
