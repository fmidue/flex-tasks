{-# OPTIONS_GHC -Wno-orphans #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}

module FlexTask.TypesSpec where


import Data.List                        (intercalate)
import Test.Hspec                       (Spec, describe, xit, shouldBe)
import Test.Hspec.Parsec                (shouldParse)
import Test.Hspec.QuickCheck            (prop, xprop)
import Test.QuickCheck                  (chooseInt, forAll, vectorOf)
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
    xit "always successfully parses when encountering 3 delimiters and no additional modules" $
      forAll (vectorOf 4 arbitrary) $ \xs ->
        parse parseFlexConfig "" (intercalate delimiter xs) `shouldParse` conf xs

  describe "both" $ do
    xprop "are inverse to each other" $ \fConf ->
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
    extraModules <- filter ((/="") . snd) <$> vectorOf amount arbitrary
    pure (CommonModules {
      globalModule,
      settingsModule,
      descriptionModule,
      parseModule,
      extraModules
      })


instance Arbitrary FlexConf where
  arbitrary = do
    taskDataModule <- arbitrary
    commonModules <- arbitrary
    pure $ FlexConf {taskDataModule, commonModules}
