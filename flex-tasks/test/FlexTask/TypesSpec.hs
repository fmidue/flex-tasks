{-# OPTIONS_GHC -Wno-orphans #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}

module FlexTask.TypesSpec where


import Data.Char                        (isLetter, toUpper)
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
        [ globalModule
        ,settingsModule
        ,taskDataModule
        ,descriptionModule
        ,parseModule
        ] ++ map snd extraModules)

  describe "parseFlexConfig" $
    it "always successfully parses when encountering 4 delimiters and no additional modules" $
      forAll (vectorOf 5 arbitrary) $ \xs ->
        parse parseFlexConfig "" (intercalate delimiter xs) `shouldParse` conf xs

  describe "both" $ do
    prop "are inverse to each other (provided extra modules are valid)" $ \fConf ->
      parse parseFlexConfig "" (showFlexConfig fConf) `shouldParse` fConf

    where
      conf [globalModule, settingsModule, taskDataModule, descriptionModule, parseModule] =
        FlexConf {
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


genValidExtraModule :: Gen (String,String)
genValidExtraModule = do
  firstChar <- toUpper <$> letter
  rest <- listOf letter
  let modName = firstChar : rest
  contents <- arbitrary
  pure (modName, "module " ++ modName ++ " where\n" ++ contents)
  where
    letter = arbitrary `suchThat` isLetter


instance Arbitrary FlexConf where
  arbitrary = do
    taskDataModule <- arbitrary
    commonModules <- arbitrary
    pure $ FlexConf {taskDataModule, commonModules}
