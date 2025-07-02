{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
{-# OPTIONS -fforce-recomp #-}

module FlexTask.DefaultConfig (defaultConfig) where


import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFileRelative)
import Text.Parsec (parse)

import FlexTask.Types (FlexConf, parseFlexConfig)




-- | Simple task configuration with textual user guide
defaultConfig :: FlexConf
defaultConfig = either (error . show) id (parse parseFlexConfig "" defaultFile)
  where
    defaultFile = unpack $(embedFileRelative "tasks/defaultConfig.flex")
