{-# OPTIONS_GHC -Wno-orphans #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}

module FlexTask.TypesSpec where


import Data.List                        (intercalate)
import Test.Hspec                       (Spec, describe, xit, shouldBe)
import Test.Hspec.Parsec                (shouldParse)
import Test.Hspec.QuickCheck            (prop, xprop)
import Test.QuickCheck                  (forAll, vectorOf)
import Test.QuickCheck.Arbitrary        (Arbitrary(..))
import Text.Parsec                      (parse)

import FlexTask.Types


spec :: Spec
spec = do
  describe "showFlexConfig" $
    prop "segments the modules with 3 sequences of delimiters" $
      \fConf@FlexConf{commonModules = CommonModules{..},..} ->
      showFlexConfig fConf `shouldBe` intercalate delimiter
        [ globalModule
        ,taskDataModule
        ,descriptionModule
        ,parseModule
        ]

  describe "parseFlexConfig" $
    xit "always successfully parses when encountering 3 delimiters" $
      forAll (vectorOf 4 arbitrary) $ \xs ->
        parse parseFlexConfig "" (intercalate delimiter xs) `shouldParse` conf xs

  describe "both" $ do
    xprop "are inverse to each other" $ \fConf ->
      parse parseFlexConfig "" (showFlexConfig fConf) `shouldParse` fConf

    where
      conf [globalModule, taskDataModule, descriptionModule, parseModule] =
        FlexConf {
          taskDataModule,
          commonModules = CommonModules {
            globalModule,
            descriptionModule,
            parseModule
          }
        }
      conf _ = error "not possible"


instance Arbitrary CommonModules where
  arbitrary = do
    globalModule <- arbitrary
    descriptionModule <- arbitrary
    parseModule <- arbitrary
    pure (CommonModules {globalModule, descriptionModule,parseModule})


instance Arbitrary FlexConf where
  arbitrary = do
    taskDataModule <- arbitrary
    commonModules <- arbitrary
    pure $ FlexConf {taskDataModule, commonModules}
