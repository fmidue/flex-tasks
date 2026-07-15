module SettingsGen where

import Test.QuickCheck.Gen


data Settings = Settings {} deriving (Eq,Show)


rollSettings :: Gen Settings
rollSettings = pure Settings {}

