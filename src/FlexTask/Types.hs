{-# LANGUAGE DeriveGeneric #-}

{- |
FlexTask configuration and task instance types.
-}

module FlexTask.Types
  ( FlexInst(..)
  , FlexConf(..)

  , getForm
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
    formFields        :: [String], -- ^ Field IDs of input elements inside the form.
    formHtml          ::  String,  -- ^ The form as Html code.
    descriptionData   ::  String,  -- ^ Data needed to produce the task Description.
    globalModule      ::  String,  -- ^ Global code module available in all interpreter runs.
    descriptionModule ::  String,  -- ^ Module for producing the task description.
    parseModule       ::  String,  -- ^ Module containing the Parser for the submission type.
    checkModule       ::  String   -- ^ Module containing the Checker functions.
  } deriving (Generic)


{- |
Configuration to use for random generation of concrete `FlexInst`.
`taskDataCode` is interpreted upon generating an instance to produce static form data.
The other Haskell modules are propagated to the generated task instance.
-}
data FlexConf = FlexConf {
    globalCode      :: String, -- ^ Same as `globalModule`. Is copied into generated `FlexInst`
    taskDataCode    :: String, -- ^ Module for generating the form, as well as `CheckModule`.
    descriptionCode :: String, -- ^ Same as `descriptionModule`. Is copied into generated `FlexInst`
    parseCode       :: String  -- ^ Same as `parseModule`. Is copied into generated `FlexInst`
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
showFlexConfig (FlexConf global taskData description parse) =
    intercalate delimiter [global, taskData, description, parse]



{- |
Parser for FlexTask configurations.
Reads four code modules each separated by at least three /equals signs/ (=).

__There is no terminator at the end of the fourth (last) module.__
__The parser will simply read until EOF__
__and interpret everything following the last separator as part of the fourth module.__
-}
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



getForm :: FlexInst -> ([String],String)
getForm (FlexInst fields html _ _ _ _ _) = (fields, html)
