{-# LANGUAGE DeriveGeneric #-}

module FlexTask.Types
  ( FlexConf(..)
  , FlexInst(..)
  , getFormData
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




data FlexInst = FlexInst {
    formFields        :: [String],
    formHtml          ::  String,
    descriptionData   ::  String,
    globalModule      ::  String,
    descriptionModule ::  String,
    parseModule       ::  String,
    checkModule       ::  String
  } deriving (Generic)


data FlexConf = FlexConf {
    globalCode      :: String,
    taskData        :: String,
    descriptionCode :: String,
    parseCode       :: String
  } deriving (Eq,Generic,Ord,Show)



delimiter :: String
delimiter = "============================================="



showFlexConfig :: FlexConf -> String
showFlexConfig (FlexConf global taskData description parse) =
    intercalate delimiter [global, taskData, description, parse]



parseFlexConfig :: Parser FlexConf
parseFlexConfig = do
      global <- untilSep
      taskData <- untilSep
      description <- untilSep
      parse <- many anyChar
      pure $ FlexConf global taskData description parse
    where
      atLeastThree = do
        void $ string "==="
        many $ char '='
      untilSep = manyTill anyChar $ try atLeastThree



getFormData :: FlexInst -> ([String],String)
getFormData (FlexInst fields html _ _ _ _ _) = (fields, html)
