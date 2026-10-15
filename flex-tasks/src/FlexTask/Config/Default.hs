{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
{-# OPTIONS -fforce-recomp #-}

module FlexTask.Config.Default (defaultConfig) where


import Data.ByteString.UTF8 (toString)
import Data.FileEmbed (embedFileRelative)
import Text.Parsec (parse)

import FlexTask.Config.Types (FlexConfig, parseFlexConfig)




-- | Simple task configuration with textual user guide
defaultConfig :: FlexConfig
defaultConfig = either (error . show) id (parse parseFlexConfig "" defaultFile)
  where
    defaultFile = toString $(embedFileRelative "tasks/defaultConfig.flex")
