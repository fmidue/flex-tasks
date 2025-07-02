{-# OPTIONS_GHC -Wno-unused-imports #-}

module Global where


import Data.Map                         (fromList)
import Tasks.ComposeFormula.Config      (ComposeFormulaInst(..))
import Trees.Types                      (BinOp(..), SynTree(..), TreeFormulaAnswer)


type Submission = [TreeFormulaAnswer]
type TaskData = ComposeFormulaInst

=============================================

module TaskSettings where


import Control.OutputCapable.Blocks     (LangM, Language(..), OutputCapable)
import Data.Map                         (Map)
import Tasks.ComposeFormula.Config (
  ComposeFormulaConfig(..),
  TreeDisplayMode (TreeDisplay),
  checkComposeFormulaConfig,
  )
import Tasks.SynTree.Config             (SynTreeConfig(..))
import Trees.Types                      (BinOp(..))
import qualified Data.Map               as Map (fromList)


-- 2024: Weight 0.33 (in Logik)
task03 :: ComposeFormulaConfig
task03 = ComposeFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 10
    , maxNodes = 14
    , minDepth = 4
    , maxDepth = 6
    , availableAtoms = "ABCDE"
    , minAmountOfUniqueAtoms = 5
    , binOpFrequencies = listToFM
      [ (And, 1)
      , (Or, 1)
      , (Impl, 1)
      , (BackImpl, 1)
      , (Equi, 1)
      ]
    , negOpFrequency = 1
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 2
    }
  , treeDisplayModes = (TreeDisplay, TreeDisplay)
  , extraText = Just $ listToFM
      [ (German, "Sie dürfen bei dieser Aufgabe nicht Klammern durch Verwendung von Assoziativität weglassen.")
      , (English, "Do not try to use associativity in order to omit brackets in this task.")
      ]
  , printSolution = True
  , offerUnicodeInput = True
  }


listToFM :: Ord k => [(k, a)] -> Map k a
listToFM = Map.fromList


validateSettings :: OutputCapable m => LangM m
validateSettings = checkComposeFormulaConfig task03

=============================================

{-# language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module TaskData where

import Data.String.Interpolate          (i)
import Data.Text                        (Text)
import FlexTask.FormUtil                (getFormData)
import FlexTask.Generic.Form
import FlexTask.Types                   (HtmlDict)
import FlexTask.YesodConfig             (Rendered, Widget)
import Tasks.ComposeFormula.Quiz        (generateComposeFormulaInst)
import Test.QuickCheck.Gen              (Gen)
import Yesod                            (RenderMessage(..), fieldSettingsLabel)

import Global                           (TaskData)
import TaskSettings                     (task03)


data InputLabel = First | Second

instance RenderMessage app InputLabel where
  renderMessage _ ("en":_) First  = "First Formula This is a test"
  renderMessage _ _        First  = "Erste Formel"
  renderMessage _ ("en":_) Second = "Second Formula"
  renderMessage _ _        Second = "Zweite Formel"


getTask :: Gen (TaskData, String, IO ([Text],HtmlDict))
getTask = do
    inst <- generateComposeFormulaInst task03
    pure (inst, checkers, getFormData form)

fields :: [[FieldInfo]]
fields = [[list Horizontal $ map fieldSettingsLabel [First, Second]]]

form :: Rendered Widget
form = formify (Nothing :: Maybe [String]) fields

checkers :: String
checkers = [i|

{-\# language ApplicativeDo \#-}

module Check where

import Control.Applicative              (Alternative)
import Control.Monad.IO.Class           (MonadIO)
import Control.OutputCapable.Blocks
import LogicTasks.Syntax.ComposeFormula (partialGrade', completeGrade')

import Global                           (TaskData, Submission)


checkSyntax
  :: OutputCapable m
  => a
  -> TaskData
  -> Submission
  -> LangM m
checkSyntax _ = partialGrade'

checkSemantics
  :: (Alternative m, MonadIO m, OutputCapable m)
  => FilePath
  -> TaskData
  -> Submission
  -> Rated m
checkSemantics = completeGrade'

|]

=============================================

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Description (description) where

import Global                           (TaskData)

import Control.Monad.IO.Class           (MonadIO)
import Control.OutputCapable.Blocks
import Tasks.ComposeFormula.Config      (ComposeFormulaInst(..))

import qualified LogicTasks.Syntax.ComposeFormula as LT

description :: (OutputCapable m, MonadIO m) => FilePath -> TaskData -> LangM m
description path inst@ComposeFormulaInst{..} = do
  LT.description False path inst
  paragraph $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise aus den folgenden beiden Eingaben bestehen: "
      english "For instance, a solution attempt could consist of the following two inputs: "
    translatedCode $ flip localise $ translations exampleCode
    pure()
  pure()
  where
    (fixedEng, fixedGer) = ("C and (A or not B)","C und (A oder nicht B)")
    exampleCode
      | unicodeAllowed = do
        english $ unlines ["(A ∨ ¬B) and C", fixedEng]
        german $ unlines ["(A ∨ ¬B) und C", fixedGer]
      | otherwise      = do
        english $ unlines ["(A or not B) and C", fixedEng]
        german $ unlines ["(A oder nicht B) und C", fixedGer]

=============================================

module Parse (parseSubmission) where


import Control.OutputCapable.Blocks (
  LangM',
  OutputCapable,
  ReportT,
  )
import Control.OutputCapable.Blocks.Generic (
  ($>>=),
  )
import FlexTask.Generic.Parse (
  displayInputAnd,
  formParser,
  parseWithFallback,
  parseInfallibly,
  )
import Formula.Parsing.Delayed (
  complainAboutMissingParenthesesIfNotFailingOn,
  )
import LogicTasks.Parsing      (formulaSymbolParser, parser)
import ParsingHelpers          (fully)

import Global                  (Submission)


parseSubmission ::
  (Monad m, OutputCapable (ReportT o m))
  => String
  -> LangM' (ReportT o m) Submission
parseSubmission input = parseInfallibly formParser input $>>= traverse parseIt
  where
    parseIt = parseWithFallback
      (fully parser)
      (displayInputAnd complainAboutMissingParenthesesIfNotFailingOn)
      (fully formulaSymbolParser)
