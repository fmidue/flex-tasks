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
    formFields          ::  [String],
    formHtml            ::   String,
    descriptionData     ::   String,
    globalModule        ::   String,
    descriptionModule   ::   String,
    parseAndCheckModule ::   String
  } deriving (Generic)


data FlexConf = FlexConf {
    globalCode        :: String,
    taskAndFormCode   :: String,
    descriptionCode   :: String,
    parseCode         :: String,
    checktemplate     :: String
  } deriving (Eq,Generic,Ord,Show)



delimiter :: String
delimiter = "============================================="



showFlexConfig :: FlexConf -> String
showFlexConfig (FlexConf global taskAndForm description parse check) =
    intercalate delimiter [global, taskAndForm, description, parse, check]



parseFlexConfig :: Parser FlexConf
parseFlexConfig = do
      global <- untilSep
      taskAndForm <- untilSep
      description <- untilSep
      parse <- untilSep
      check <- many anyChar
      pure $ FlexConf global taskAndForm description parse check
    where
      atLeastThree = do
        void $ string "==="
        many $ char '='
      untilSep = manyTill anyChar $ try atLeastThree



getFormData :: FlexInst -> ([String],String)
getFormData (FlexInst fields html _ _ _ _) = (fields, html)
