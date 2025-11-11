{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  , validateFlexConfig
  ) where


import Control.Monad                     (void)
import Control.OutputCapable.Blocks      (LangM, OutputCapable, indent, refuse, translate, german, english)
import Data.Char                         (isAscii, isLetter, isNumber)
import Data.List.Extra (
  dropEnd1,
  intercalate,
  isPrefixOf,
  notNull,
  nubOrd,
  stripInfix,
  word1
  )
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
    many1,
    manyTill,
    option,
    satisfy,
    string,
    skipMany,
    try,
    sepBy,
    space,
    spaces,
    )
import Text.Parsec.Char                  (endOfLine, oneOf)
import Text.Parsec.String                (Parser)
import Yesod                             (Lang)



-- | A map of language code and internationalized HTML value pairs.
type HtmlDict = Map Lang String


{- |
Concrete Task instance.
Contained Haskell code is runtime interpreted to produce needed components of a task.
-}
data FlexInst = FlexInst {
    form            :: ([[Text]],HtmlDict), -- ^ Field IDs of input elements and Html code.
    taskData        ::  String,             -- ^ Flexible task data used by task description and checker functions.
    commonModules   ::  CommonModules,      -- ^ Modules shared between config and instance.
    checkModule     ::  String              -- ^ Module containing the Checker functions.
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
    taskName          ::  String, -- ^ A task identifier used as a label for file caching
    globalModule      ::  String, -- ^ Global code module available in all interpreter runs.
    settingsModule    ::  String, -- ^ Module for task configuration constants.
    descriptionModule ::  String, -- ^ Module for producing the task description.
    parseModule       ::  String, -- ^ Module containing the Parser for the submission type.
    extraModules      :: [(String,String)] -- ^ User defined additional modules with format (Name,Code)
  } deriving (Eq,Generic,Ord,Show)



-- | Visual module separator for configuration display.
delimiter :: String
delimiter = "\r\n=============================================\r\n"



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
      ["taskName: " ++ taskName ++ "\r\n" | notNull taskName] ++
      [ globalModule
      , settingsModule
      , taskDataModule
      , descriptionModule
      , parseModule
      ]
      ++ map snd extraModules



{- |
Parser for FlexTask configurations.
Reads five or more code modules each separated by at least three /equals signs/ (=).

Modules starting from the sixth will be added to `CommonModules.extraModules`.
-}
parseFlexConfig :: Parser FlexConf
parseFlexConfig = do
    taskName <- option "" $ try parsePathSegment
    modules <- betweenEquals
    case splitAt 5 modules of
      ( [ globalModule
        , settingsModule
        , taskDataModule
        , descriptionModule
        , parseModule
        ], extra) -> do
        let extraModules = mapMaybe getModName extra
        pure $
          FlexConf {
            taskDataModule,
            commonModules = CommonModules {
              taskName,
              globalModule,
              settingsModule,
              descriptionModule,
              parseModule,
              extraModules
            }
          }
      _ -> fail $
        "Unexpected end of file. " ++
        "Provide at least the following Modules (in this order): " ++
        "Global, TaskSettings, TaskData (Check), Description, Parse"
  where
    atLeastThree = do
      discard endOfLine
      lexeme $ string "===" >> skipMany (char '=')
      void endOfLine

    betweenEquals =
      manyTill anyChar (try $ lookAhead $ eof <|> atLeastThree) `sepBy`
      atLeastThree

    parsePathSegment = do
      spaces
      discardString "taskName"
      discardString ":"
      path <- lexeme $ many1 $ satisfy $ liftA2 (&&) isAscii (liftA2 (||) isLetter isNumber)
      void $ manyTill space $ try $ lookAhead atLeastThree
      atLeastThree
      pure path

    -- the Parsec provided 'spaces' parser also parses newline characters
    parseSpace = skipMany $ oneOf [' ', '\t']
    lexeme = (<* parseSpace)
    discard = void . lexeme
    discardString = discard . string


getModName :: String -> Maybe (String, String)
getModName code = do
  (_,nameAtFront) <- stripInfix "module" $ removeComments code
  Just (fst $ word1 nameAtFront, code)


removeComments :: String -> String
removeComments = unlines . filter (not . ("--" `isPrefixOf`)) . lines . runRemove
  where
    runRemove xs = case stripInfix "{-" xs of
      Nothing -> xs
      Just (a,b) -> a ++ case stripInfix "-}" b of
        Nothing -> xs
        Just (_,rest) -> runRemove rest


-- | Check a configuration for inconsistencies
validateFlexConfig :: OutputCapable m => FlexConf -> LangM m
validateFlexConfig FlexConf{commonModules = CommonModules{..},..}
  | requiredNames /= requiredConfig = reject $ do
    german $
      "Die festen Module wurden in Reihenfolge oder Namen verändert. " ++
      "Sie müssen exakt mit folgender Reihenfolge und " ++
      "folgenden Bezeichnern definiert werden: " ++
      listRequired
    english $
      "The names or order of required modules was changed. " ++
      "They have to be defined with exactly the following names and order: " ++
      listRequired
  | "Helper" `elem` moduleNames = reject $ do
    german $
      "Eines der zusätzlichen Module wurde mit Namen \"Helper\" definiert. " ++
      "Dieser Name ist für ein internes Modul reserviert."
    english $
      "An additional Module was defined as \"Helper\". " ++
      "This name is reserved for internal use."
  | any (`elem` required) moduleNames = reject $ do
    german "Eines der Zusatzmodule wurde wie ein festes Modul benannt."
    english "An additional module has the same name as a required one."
  | nubOrd moduleNames /= moduleNames = reject $ do
    german "Mindestens zwei Zusatzmodule haben den gleichen Namen."
    english "At least two additional modules use the same name."
  | otherwise = pure ()
  where
    reject = refuse . indent . translate
    moduleNames = map fst extraModules
    required = ["Global","TaskSettings","TaskData","Description","Parse","Check"]
    requiredConfig = dropEnd1 required
    listRequired = intercalate ", " requiredConfig
    requiredNames = mapMaybe (fmap fst . getModName)
      [ globalModule
      , settingsModule
      , taskDataModule
      , descriptionModule
      , parseModule
      ]
