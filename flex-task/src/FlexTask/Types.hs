{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{- |
FlexTask configuration and task instance types.
-}

module FlexTask.Types
  ( CommonModules(..)
  , FlexInst(..)
  , FlexConf(..)

  , parseFlexConfig
  , showFlexConfig
  ) where


import Control.Monad                     (void)
import Data.List                         (intercalate)
import GHC.Generics                      (Generic)
import Text.Parsec (
    anyChar,
    char,
    many,
    manyTill,
    string,
    try
    )
import Text.Parsec.String                (Parser)




{- |
Concrete Task instance.
Contained Haskell code is runtime interpreted to produce needed components of a task.
-}
data FlexInst = FlexInst {
    form            :: ([String],String), -- ^ Field IDs of input elements and form as Html code.
    taskData        ::  String,           -- ^ Flexible task data used by task description and checker functions.
    commonModules   ::  CommonModules,    -- ^ Modules shared between config and instance.
    checkModule     ::  String            -- ^ Module containing the Checker functions.
  } deriving (Generic)


{- |
Configuration to use for random generation of concrete `FlexInst`.
`taskDataModule` is interpreted upon generating an instance to produce static form data.
The other Haskell modules are propagated to the generated task instance.
-}
data FlexConf = FlexConf {
    taskDataModule :: String,       -- ^ Module for generating the form, as well as `CheckModule`.
    commonModules  :: CommonModules -- ^ Modules shared between config and instance.
  } deriving (Eq,Generic,Ord,Show)


{- |
Modules present in both `FlexConf` and `FlexInst`.
They are propagated to the generated task instance.
-}
data CommonModules = CommonModules {
    globalModule      ::  String, -- ^ Global code module available in all interpreter runs.
    descriptionModule ::  String, -- ^ Module for producing the task description.
    parseModule       ::  String  -- ^ Module containing the Parser for the submission type.
  } deriving (Eq,Generic,Ord,Show)



delimiter :: String
delimiter = "============================================="



{- |
Convert a configuration into a String.
The modules are separated by lines of at least three consecutive /equals signs/ (=).
e.g.

@
Module1 where
...

====================

Module2 where
...
@
-}
showFlexConfig :: FlexConf -> String
showFlexConfig FlexConf{commonModules = CommonModules{..},..} =
    intercalate delimiter
      [ globalModule
      , taskDataModule
      , descriptionModule
      , parseModule
      ]



{- |
Parser for FlexTask configurations.
Reads four code modules each separated by at least three /equals signs/ (=).

__There is no terminator at the end of the fourth (last) module.__
__The parser will simply read until EOF__
__and interpret everything following the last separator as part of the fourth module.__
-}
parseFlexConfig :: Parser FlexConf
parseFlexConfig = do
      globalModule <- untilSep
      taskDataModule <- untilSep
      descriptionModule <- untilSep
      parseModule <- many anyChar
      pure $
        FlexConf {
          taskDataModule,
          commonModules = CommonModules {
            globalModule,
            descriptionModule,
            parseModule
          }
        }
    where
      atLeastThree = do
        void $ string "==="
        many $ char '='
      untilSep = manyTill anyChar $ try atLeastThree
