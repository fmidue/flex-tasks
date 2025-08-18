{-# OPTIONS_GHC -Wno-orphans #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}

module FlexTask.TypesSpec where


import Data.Char                        (isAscii, isLetter, toUpper)
import Data.List                        (intercalate)
import Test.Hspec                       (Spec, describe, it, shouldBe)
import Test.Hspec.Parsec                (shouldParse)
import Test.Hspec.QuickCheck            (prop)
import Test.QuickCheck (
  Gen,
  chooseInt,
  forAll,
  suchThat,
  listOf,
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
        [ "taskName: " ++ taskName ++ "\r\n"
        , globalModule
        , settingsModule
        , taskDataModule
        , descriptionModule
        , parseModule
        ] ++ map snd extraModules)

  describe "parseFlexConfig" $
    it "always successfully parses when encountering 5 delimiters and no additional modules" $
      forAll (vectorOf 5 arbitrary) $ \xs ->
        forAll genValidName $ \tName ->
          parse parseFlexConfig "" (intercalate delimiter (("taskName: " ++ tName):xs)) `shouldParse`
          conf (tName:xs)

  describe "both" $ do
    prop "are inverse to each other (provided extra modules are valid)" $ \fConf ->
      parse parseFlexConfig "" (showFlexConfig fConf) `shouldParse` fConf

    where
      conf [taskName, globalModule, settingsModule, taskDataModule, descriptionModule, parseModule] =
        FlexConf {
          taskName,
          taskDataModule,
          commonModules = CommonModules {
            globalModule,
            settingsModule,
            descriptionModule,
            parseModule,
            extraModules = []
          }
        }
      conf _ = error "not possible"


instance Arbitrary CommonModules where
  arbitrary = do
    globalModule <- arbitrary
    settingsModule <- arbitrary
    descriptionModule <- arbitrary
    parseModule <- arbitrary
    amount <- chooseInt (1,5)
    extraModules <- vectorOf amount genValidExtraModule
    pure (CommonModules {
      globalModule,
      settingsModule,
      descriptionModule,
      parseModule,
      extraModules
      })


genValidName :: Gen String
genValidName = listOf (letter `suchThat` isAscii)


genValidExtraModule :: Gen (String,String)
genValidExtraModule = do
  firstChar <- toUpper <$> letter
  rest <- listOf letter
  let modName = firstChar : rest
  contents <- arbitrary
  pure (modName, "module " ++ modName ++ " where\n" ++ contents)


letter :: Gen Char
letter = arbitrary `suchThat` isLetter


instance Arbitrary FlexConf where
  arbitrary = do
    taskName <- genValidName
    taskDataModule <- arbitrary
    commonModules <- arbitrary
    pure $ FlexConf {taskName, taskDataModule, commonModules}
