taskName: ExamSeatInfo

=============================================

{-# language DeriveGeneric #-}
{-# language OverloadedLists #-}
{-# language RecordWildCards #-}

module Global where


import GHC.Generics                     (Generic)


data Location = LX1205 Block | LX1203 deriving (Show, Eq)
data Block = LeftSide | Middle | RightSide deriving (Show, Eq)


data SeatDescription = SeatDescription {
  location :: Location,
  row :: Integer,
  seat :: Integer
  } deriving (Show, Eq)


translatedDescription :: SeatDescription -> (String,String)
translatedDescription SeatDescription{..} =
    ( english ++ ", row " ++ show row ++ ", seat " ++ show seat ++ " from the left"
    , german ++ ", Reihe " ++ show row ++ ", Sitz " ++ show seat ++ " von links"
    )
  where
    (english,german) = locationToTranslation location


locationToTranslation :: Location -> (String,String)
locationToTranslation LX1203 = ("Room LX1203", "Raum LX1203")
locationToTranslation (LX1205 block) = ("Room LX1205, " ++ english, "Raum LX1205, " ++ german)
  where
    (english, german) = blockToTranslation block


blockToTranslation :: Block -> (String,String)
blockToTranslation LeftSide  = ("left block", "linker Block")
blockToTranslation Middle    = ("middle block", "mittlerer Block")
blockToTranslation RightSide = ("right block", "rechter Block")


newtype MatriculationNumber = MatriculationNumber Integer deriving (Generic,Eq)


type Submission = MatriculationNumber
type TaskData = ()

=============================================

module TaskSettings where


import Control.OutputCapable.Blocks (LangM, OutputCapable, indent, refuse, translate, german, english)

import Global



seatingArrangement :: [(Integer,SeatDescription)]
seatingArrangement = zip shuffledAttendees $ fullLX1203 ++ partialLX1205



{-
Pattern used for seating arrangement:

X-X-X...
-----...
X-X-X...

i.e. leaving one seat and one row empty between students
-}

-- 48 seats, full capacity
fullLX1203 :: [SeatDescription]
fullLX1203 =
  [SeatDescription LX1203 r s | r <- [1,3..13], s <- [1,3..13]
  , (r,s) /= (7,7) -- no seat there due to projector
  ]


-- 202 seats
-- leaving empty the last row for the right and middle blocks
partialLX1205 :: [SeatDescription]
partialLX1205 =
  [SeatDescription (LX1205 LeftSide) r s | r <- [1,3..23], s <- [2,4..10]] ++
  [SeatDescription (LX1205 Middle) r s | r <- [1,3..21], s <- [1,3..16]] ++
  [SeatDescription (LX1205 RightSide) r s | r <- [1,3..21], s <- [1,3..9]]


-- attendees (order should be randomized beforehand)
shuffledAttendees :: [Integer]
shuffledAttendees = [3000001..3000250] -- dummy values


validateSettings :: OutputCapable m => LangM m
validateSettings
  | length seatingArrangement < length shuffledAttendees =
      refuse $ indent $ translate $ do
        english "there's too few seats. Some students will not be assigned one!"
        german "Es gibt nicht genügend Sitzplätze. Einigen Studenten kann kein Platz zugewiesen werden!"
  | otherwise = pure ()

=============================================

{-# language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module TaskData (getTask) where


import FlexTask.Generic.Form
import FlexTask.YesodConfig    (Rendered, Widget)
import Data.String.Interpolate (i)
import Yesod                   (RenderMessage(..), fieldSettingsLabel)

import Global
import TaskSettings


data Label = MatriculationNumberInput



instance RenderMessage a Label where
  renderMessage _ ("de":_) MatriculationNumberInput = "Matrikelnummer"
  renderMessage _ _        MatriculationNumberInput = "Matriculation number"


instance Formify MatriculationNumber


getTask :: Monad m => m (TaskData, String, Rendered Widget)
getTask = pure ((), checkers, form)


form :: Rendered Widget
form = formify
  (Nothing :: Maybe Submission)
  [[single $ fieldSettingsLabel MatriculationNumberInput]]


checkers :: String
checkers = [i|


{-\# language ApplicativeDo \#-}

module Check (checkSyntax, checkSemantics) where


import Control.OutputCapable.Blocks

import Global



checkSyntax :: OutputCapable m => TaskData -> Submission -> LangM m
checkSyntax _ _  = pure ()  -- nothing to check here


checkSemantics :: OutputCapable m => FilePath -> TaskData -> Submission -> Rated m
checkSemantics _ _ (MatriculationNumber num) = case lookup num #{seatingArrangement} of
  Nothing      -> do
    refuse $ paragraph $ translate $ do
      german $
        "Die angegebene Matrikelnummer wurde nicht gefunden. " ++
        "Überprüfen Sie bitte genau, ob Ihre Eingabe korrekt ist und ob Sie für die Klausur angemeldet sind."
      english $
        "Could not find the provided matriculation number. " ++
        "Please ensure your input is correct and you are registered for the exam."
    pure 0
  Just seating -> do
    paragraph $ translate $ do
      german "Sie sind angemeldet."
      english "You are registered."
    paragraph $ do
      translate $ do
        german "Ihre Sitzplatz-Beschreibung lautet:"
        english "Your seating information is:"
      translateCode $ do
        let (e,g) = translatedDescription seating
        english e
        german g
      pure ()
    paragraph $ translate $ do
        german "Richtungsangaben beziehen sich auf die Perspektive bei Eintritt. "
        german "Reihe 1 befindet sich vor dem Podium und Reihen im Raum sind zusätzlich mit ihrer Nummer markiert."
        english "Given directions refer to the perspective upon entry. "
        english "Row 1 is in front of the stage and the rows are also labeled with their number in the room."
    pure 1

|]

=============================================

{-# Language ApplicativeDo #-}

module Description (description) where


import Control.OutputCapable.Blocks
import Data.String.Interpolate                   (i)

import Global



description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ _ = do
  paragraph $ translate $ do
    german "Mittels dieser Aufgabe erhalten Sie Informationen über Ihren Sitzplatz in der Klausur."
    english "You will be given information concerning your seat during the exam through this task."
  paragraph $ translate $ do
    german "Geben Sie dafür Ihre Matrikelnummer in das gegebene Textfeld ein. "
    english "Enter your matriculation number into the given text field. "
    german $
      "Wenn Ihre Nummer für die Klausur angemeldet ist, " ++
      "enthält das anschließende Feedback eine Beschreibung Ihres Sitzplatzes."
    english "The resulting feedback will contain a description of your seat if your number is registered for the exam."
  pure ()

=============================================

module Parse (parseSubmission) where


import Control.Monad                    (void)
import Control.OutputCapable.Blocks (
  LangM',
  ReportT,
  OutputCapable,
  )
import FlexTask.Generic.Parse  (
  Parse(..),
  escaped,
  parseWithOrReport,
  reportWithFieldNumber,
  )
import Text.Parsec

import Global


{-
only digits, no letters
begins with 3 (or 2)
next digit is 0, 1 or 2
always 7 digits long

We found a student with a number starting with 2 during this task.
This number still incidentally continues with another 2 afterwards,
but it is unclear, if those numbers always contain a 0, 1 or 2 in their second digit.
The parsing rules should therefore be checked again and adjusted if necessary when this task is next deployed.
-}
instance Parse MatriculationNumber where
  formParser = escaped $ do
    prefix <- char '2' <|>  char '3'
    next <- oneOf ['0', '1', '2'] <|>
      -- stop-gap to prevent submission delimiters leaking into ParseError
      (char '"' >> unexpected "end of input")
      <?> "digits 0, 1 or 2"
    rest <- many (digit <?> "7 digits")
    if length rest < 5
      then unexpected "end of input"
      else if length rest > 5
        then unexpected "additional digits"
        else pure (MatriculationNumber $ read $ prefix:next:rest)

parseSubmission ::
  (Monad m, OutputCapable (ReportT o m))
  => String
  -> LangM' (ReportT o m) Submission
parseSubmission = parseWithOrReport formParser reportWithFieldNumber
