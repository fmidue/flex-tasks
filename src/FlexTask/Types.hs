{-# LANGUAGE DeriveGeneric #-}

module FlexTask.Types
  ( FlexConf(..)
  , FlexInst(..)
  , getFormData
  , parseFlexConfig
  , showFlexConfig
  ) where


import Control.Monad                     (void)
import Control.OutputCapable.Blocks.Type (Output)
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




data FlexInst = FlexInst {
    desc                ::  [Output],
    formFields          ::  [String],
    formHtml            ::   String,
    globalModule        ::   String,
    parseAndCheckModule ::   String
  } deriving (Generic)


data FlexConf = FlexConf {
    globalCode        :: String,
    taskAndFormCode   :: String,
    parseCode         :: String,
    checktemplate     :: String
  } deriving (Eq,Generic,Ord,Show)



delimiter :: String
delimiter = "============================================="



showFlexConfig :: FlexConf -> String
showFlexConfig (FlexConf global taskAndForm parse check) =
    intercalate delimiter [global, taskAndForm, parse, check]



parseFlexConfig :: Parser FlexConf
parseFlexConfig = do
      global <- untilSep
      taskAndForm <- untilSep
      parse <- untilSep
      check <- many anyChar
      pure $ FlexConf global taskAndForm parse check
    where
      atLeastThree = do
        void $ string "==="
        many $ char '='
      untilSep = manyTill anyChar $ try atLeastThree



getFormData :: FlexInst -> ([String],String)
getFormData (FlexInst _ fields html _ _) = (fields, html)
