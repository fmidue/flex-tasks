{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{- |
FlexTask configuration and task instance types.
-}

module FlexTask.Types
  ( HtmlDict
  , CommonModules(..)
  , FlexInst(..)
  , FlexConf(..)
  , delimiter

  , parseFlexConfig
  , showFlexConfig
  ) where


import Control.Monad                     (void)
import Data.List.Extra                   (headDef, intercalate, stripInfix)
import Data.Map                          (Map)
import Data.Maybe                        (mapMaybe)
import Data.Text                         (Text)
import GHC.Generics                      (Generic)
import Text.Parsec (
    (<|>),
    anyChar,
    char,
    eof,
    lookAhead,
    many,
    manyTill,
    string,
    try,
    sepBy,
    )
import Text.Parsec.String                (Parser)
import Yesod                             (Lang)



-- | A map of language code and internationalized HTML value pairs.
type HtmlDict = Map Lang String


{- |
Concrete Task instance.
Contained Haskell code is runtime interpreted to produce needed components of a task.
-}
data FlexInst = FlexInst {
    form            :: ([Text],HtmlDict), -- ^ Field IDs of input elements and Html code.
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
    parseModule       ::  String, -- ^ Module containing the Parser for the submission type.
    extraModules      :: [(String,String)] -- ^ User defined additional modules with format (Name,Code)
  } deriving (Eq,Generic,Ord,Show)



-- | Visual module separator for configuration display.
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
    intercalate delimiter $
      [ globalModule
      , taskDataModule
      , descriptionModule
      , parseModule
      ]
      ++ map snd extraModules



{- |
Parser for FlexTask configurations.
Reads four code modules each separated by at least three /equals signs/ (=).

__There is no terminator at the end of the fourth (last) module.__
__The parser will simply read until EOF__
__and interpret everything following the last separator as part of the fourth module.__
-}
parseFlexConfig :: Parser FlexConf
parseFlexConfig = do
      modules <- betweenEquals
      case take 4 modules of
        [globalModule,taskDataModule,descriptionModule,parseModule] -> do
          let extra = drop 4 modules
          let extraModules = mapMaybe getModNames extra
          pure $
            FlexConf {
              taskDataModule,
              commonModules = CommonModules {
                globalModule,
                descriptionModule,
                parseModule,
                extraModules
              }
            }
        _ -> fail $
               "Unexpected end of file. " ++
               "Provide at least the following Modules (in this order): " ++
               "Global, TaskData, Description, Parse"
    where
      atLeastThree = do
        void $ string "==="
        void $ many $ char '='
      betweenEquals =
        manyTill anyChar (try $ lookAhead $ eof <|> atLeastThree) `sepBy`
        atLeastThree
      getModNames code = (\x -> (headDef "" $ words $ snd x, code)) <$>
        stripInfix "module" code
