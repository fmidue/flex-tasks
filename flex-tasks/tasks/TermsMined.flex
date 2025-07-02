{-# OPTIONS_GHC -Wno-unused-imports #-}

module Global where


import TermTasks.Records (
  SigInstance(..),
  )
import TermTasks.DataType (
  Symbol(..),
  Term(..),
  Type(..),
  )


type Submission = [Int]
type TaskData = SigInstance

=============================================

module TaskSettings where


import Control.OutputCapable.Blocks
import Data.Foldable                    (traverse_)
import TermTasks.Direct                 (verifyCertain)
import TermTasks.Records (
  Certain(..),
  Base(..),
  )
import TermTasks.DataType (
  Error(..),
  Signature,
  )
import Data.Functor (
  (<&>),
  )
import Test.QuickCheck.Gen (
  Gen,
  elements,
  )

import Choices                         (sigList)

-- 2024: Weight 0.15 (in Modellierung)
task04 :: Gen Certain
task04 = elements sigList <&> makeConfig


-- Making the config to use for this task. Signatures will be randomly picked out of a pool of options.
makeConfig :: Signature -> Certain
makeConfig sig = Certain {
  signatures = sig,
  root = Nothing,
  baseConf = Base {
    termSizeRange = ( 6, 10 ),
    wrongTerms = [(1, Swap), (1, TypeChange), (1, OneMore), (1, OneLess)],
    properTerms = 5,
    extraFeedback = True,
    printSolution = True,
    extraText = Nothing
  }
}


validateSettings :: OutputCapable m => LangM m
validateSettings = traverse_ (verifyCertain . makeConfig) sigList

=============================================

{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module TaskData where

import Data.String.Interpolate          (i)
import Data.Text                        (Text)
import FlexTask.FormUtil                (getFormData, universalLabel)
import FlexTask.Generic.Form
import FlexTask.Types                   (HtmlDict)
import TermTasks.DataType               (inMathit)
import TermTasks.Direct                 (genInst)
import TermTasks.Records (
  SigInstance(..),
  )
import Test.QuickCheck.Gen              (Gen)
import Yesod                            (RenderMessage(..), fieldSettingsLabel)

import Global                           (TaskData)
import TaskSettings                     (task04)


data HeaderLabel = HeaderLabel


instance RenderMessage app HeaderLabel where
  renderMessage _ ("en":_) HeaderLabel  = "Correct Terms"
  renderMessage _ _        HeaderLabel  = "Korrekte Terme"


getTask :: Gen (TaskData, String, IO ([Text],HtmlDict))
getTask = do
    config <- task04
    inst <- genInst config
    pure (inst, checkers, getFormData $ form inst)
  where
    latexLabel = zipWith
      (\n -> universalLabel . (show n ++) . (". \\(" ++) . (++ "\\)") . inMathit)
      [1 :: Int ..]
      . terms
    form inst = formify
      (Nothing :: Maybe MultipleChoiceSelection)
      [[buttons Vertical (fieldSettingsLabel HeaderLabel) $ latexLabel inst]]


checkers :: String
checkers = [i|

module Check where


import Control.Applicative              (Alternative)
import Control.OutputCapable.Blocks
import TermTasks.Direct                 (completeGrade, partialGrade)

import Global                           (Submission, TaskData)



checkSyntax
  :: OutputCapable m
  => a
  -> TaskData
  -> Submission
  -> LangM m
checkSyntax _ = partialGrade


checkSemantics
  :: (Alternative m, OutputCapable m)
  => a
  -> TaskData
  -> Submission
  -> Rated m
checkSemantics _ = completeGrade

|]

=============================================

module Description (description) where

import Control.OutputCapable.Blocks
import qualified TermTasks.Direct       as TD

import Global                           (TaskData)



description :: OutputCapable m => a -> TaskData -> LangM m
description _ = TD.description False

=============================================

module Parse (parseSubmission) where


import Data.Functor            ((<&>))
import FlexTask.Generic.Form   (getAnswers)
import FlexTask.Generic.Parse  (formParser, parseInfallibly)

import Global                  (Submission)



parseSubmission ::
  Applicative m
  => String
  -> m Submission
parseSubmission input = parseInfallibly formParser input <&> getAnswers


=============================================

module Choices where


import TermTasks.DataType (Signature, toSignature)



-- Possible signatures to roll for the task
sigList :: [Signature]
sigList = [

{- all too long:

  -- ekg-push-1.json -- no-spell-check
  toSignature [
      ("defaultPushOptions", [], "PushOptions"),
      ("prefix", ["PushOptions"], "Text"),
      ("suffix", ["PushOptions"], "Text"),
      ("debug", ["PushOptions"], "Bool"),
      ("flushInterval", ["PushOptions"], "Int"),
      ("makeOptions", ["Int", "Bool", "Text", "Text"], "PushOptions")
      ],

  -- Euterpea-1.json -- no-spell-check
  toSignature [
      ("trackEnd", [], "Message"),
      ("key", ["Message"], "Key"),
      ("controllerNumber", ["Message"], "Int"),
      ("channel", ["Message"], "Channel"),
      ("controlChange", ["Channel", "Int", "Int"], "Message"),
      ("timeSignature", ["Int", "Int", "Int", "Int"], "Message")
      ],

  -- hS3-1.json -- no-spell-check
  toSignature [
      ("defaultAmazonS3Port", [], "Int"),
      ("defaultAmazonS3Host", [], "String"),
      ("makeAWSConnection", ["String", "Int", "String", "String"], "AWSConnection"),
      ("awsPort", ["AWSConnection"], "Int"),
      ("amazonS3Connection", ["String", "String"], "AWSConnection"),
      ("awsAccessKey", ["AWSConnection"], "String")
      ],

  -- unliftio-path-1.json -- no-spell-check
  toSignature [
      ("emptyPermissions", [], "Permissions"),
      ("exeExtension", [], "String"),
      ("readable", ["Permissions"], "Bool"),
      ("setOwnerReadable", ["Bool", "Permissions"], "Permissions"),
      ("setOwnerSearchable", ["Bool", "Permissions"], "Permissions"),
      ("executable", ["Permissions"], "Bool")
      ],
-}

  -- Cabal-syntax-2.json -- no-spell-check
  toSignature [
      ("zeroPos", [], "Position"),
      ("retPos", ["Position"], "Position"),
      ("showPos", ["Position"], "String"),
      ("positionRow", ["Position"], "Int"),
      ("makePosition", ["Int", "Int"], "Position"),
      ("incPos", ["Int", "Position"], "Position")
      ],

  -- french-cards-1.json -- no-spell-check
  toSignature [
      ("rTwo", [], "Rank"),
      ("diamonds", [], "Suit"),
      ("newCard", ["Rank", "Suit"], "Card"),
      ("cardRank", ["Card"], "Rank"),
      ("cardWord", ["Card"], "Word8"),
      ("cardSuit", ["Card"], "Suit")
      ],

  -- ghc-26.json -- no-spell-check
  toSignature [
      ("boxedTuple", [], "TupleSort"),
      ("finalPhase", [], "CompilerPhase"),
      ("activeAfter", ["CompilerPhase"], "Activation"),
      ("isAlwaysActive", ["Activation"], "Bool"),
      ("isActive", ["CompilerPhase", "Activation"], "Bool"),
      ("beginPhase", ["Activation"], "CompilerPhase")
      ],

  -- haskellscrabble-2.json -- no-spell-check
  toSignature [
      ("emptyBoard", [], "Board"),
      ("posMax", [], "Int"),
      ("prettyPrint", ["Board"], "String"),
      ("makePos", ["Int", "Int", "String"], "Pos"),
      ("yPos", ["Pos"], "Int"),
      ("gridValue", ["Pos"], "String")
      ],

  -- LPFP-core-3.json -- no-spell-check
  toSignature [
      ("defaultPFS", [], "ParticleFieldState"),
      ("position", ["ParticleFieldState"], "Position"),
      ("mass", ["ParticleFieldState"], "R"),
      ("scalePos", ["R", "Position"], "Position"),
      ("pfsUpdate", ["R", "ParticleFieldState"], "ParticleFieldState"),
      ("charge", ["ParticleFieldState"], "R")
      ],

  -- packedstring-2.json -- no-spell-check
  toSignature [
      ("nilPS", [], "PackedString"),
      ("lengthPS", ["PackedString"], "Int"),
      ("appendPS", ["PackedString", "PackedString"], "PackedString"),
      ("dropPS", ["Int", "PackedString"], "PackedString"),
      ("nullPS", ["PackedString"], "Bool"),
      ("unpackPS", ["PackedString"], "String")
      ],

  -- prettyprint-avh4-2.json -- no-spell-check
  toSignature [
      ("space", [], "Line"),
      ("blankLine", [], "Block"),
      ("line", ["Line"], "Block"),
      ("indent", ["Block"], "Block"),
      ("render", ["Block"], "Builder"),
      ("addSuffix", ["Line", "Block"], "Block")
      ],

  -- protobuf-1.json -- no-spell-check
  toSignature [
      ("empty", [], "Builder"),
      ("append", ["Builder", "Builder"], "Builder"),
      ("size", ["Builder"], "Int"),
      ("toLazyByteString", ["Builder"], "ByteString"),
      ("fromByteString", ["ByteString"], "Builder"),
      ("makeBuilder", ["Int", "Builder"], "Builder")
      ],

  -- template-toolkit-1.json -- no-spell-check
  toSignature [
      ("undef", [], "Val"),
      ("not", [], "UnOp"),
      ("eVal", ["Val"], "Expr"),
      ("eUnOp", ["UnOp", "Expr"], "Expr"),
      ("eTerOp", ["Expr", "Expr", "Expr"], "Expr"),
      ("vArrayRange", ["Expr", "Expr"], "Val")
      ],

  -- time-1.json -- no-spell-check
  toSignature [
      ("midnight", [], "TimeOfDay"),
      ("midday", [], "TimeOfDay"),
      ("todHour", ["TimeOfDay"], "Int"),
      ("todSec", ["TimeOfDay"], "Pico"),
      ("makeTime", ["Int", "Int", "Pico"], "TimeOfDay"),
      ("sinceMidnight", ["TimeOfDay"], "DiffTime")
      ],

  -- web-rep-2.json -- no-spell-check
  toSignature [
      ("defaultSocketConfig", [], "SocketConfig"),
      ("defaultSocketPage", [], "Page"),
      ("path", ["SocketConfig"], "Text"),
      ("host", ["SocketConfig"], "Text"),
      ("port", ["SocketConfig"], "Int"),
      ("makeConfig", ["Text", "Int", "Text"], "SocketConfig")
      ]
  ]

