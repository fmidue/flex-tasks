module SettingsGen where

import Test.QuickCheck.Gen


data Settings = Settings {
  emptyColumns :: Int,
  tableRequired :: Bool,
  showSolution :: Bool
  } deriving (Eq,Show)


rollSettings :: Gen Settings
rollSettings = Settings <$> chooseInt (5,14) <*> chooseAny <*> chooseAny

