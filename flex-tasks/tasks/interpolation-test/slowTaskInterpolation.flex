module Global where


type Submission = (Int,Int)
type TaskData = [Int]

=============================================

module TaskSettings where


import Control.OutputCapable.Blocks     (LangM, OutputCapable)


validateSettings :: OutputCapable m => LangM m
validateSettings = pure ()

=============================================

{-# language OverloadedStrings #-}
{-# Language QuasiQuotes #-}
module TaskData (getTask) where


import Data.String.Interpolate (i)
import Data.Text               (Text)
import FlexTask.FormUtil       (getFormData)
import FlexTask.Generic.Form
import FlexTask.Types          (HtmlDict)
import FlexTask.YesodConfig    (Rendered, Widget)
import Test.QuickCheck.Gen

import Global




getTask :: Gen (TaskData, String, IO ([Text],HtmlDict))
getTask = do
    numbers <- vectorOf 15 $ chooseInt (0,1000)
    pure (numbers, checkers numbers, getFormData form)



fields :: [[FieldInfo]]
fields = [[single "Number one"], [single "Number two"]]


form :: Rendered Widget
form = formify (Nothing :: Maybe Submission) fields


checkers :: [Int] -> String
checkers solution = [i|

{-\# language ApplicativeDo \#-}

module Check (checkSemantics, checkSyntax) where


import Control.OutputCapable.Blocks

import Global



checkSyntax :: OutputCapable m => FilePath -> a -> Submission -> LangM m
checkSyntax _ _ (a,b)= do
    assertion (all (`elem` #{solution}) [a,b]) $ --ignore-length
      translate $ do
        german "Das Tupel kommt im Kreuzprodukt vor?"
        english "Tuple is inside the cross product?"
    pure ()


checkSemantics :: OutputCapable m => FilePath -> a -> Submission -> Rated m
checkSemantics _ _ (a,b) = do
    assertion (all (`elem` #{noNegNeighbours}) [a,b]) $ --ignore-length
      translate $ do
        german "Mindestens ein Nachbar ist positiv?"
        english "At least one Neighbour is positive?"
    assertion (all (`elem` #{noDiv3Neighbours}) [a,b]) $ --ignore-length
      translate $ do
        german "Nachbarn sind nicht durch drei teilbar?"
        english "Neighbours are not divisible by three?"
    pure 1.0
|]
  where
    neighbourTups = zip3 solution (drop 1 solution) (drop 2 solution)
    fromNeighbourTups f = map (\(_,b,_) -> b) $ filter f neighbourTups
    noNegNeighbours = fromNeighbourTups (\(a,_,c) -> a >= 0 || c >= 0)
    noDiv3Neighbours = fromNeighbourTups (\(a,_,c) -> a `mod` 3 /= 0 || c `mod` 3 /= 0)

=============================================

{-# Language ApplicativeDo #-}

module Description (description) where


import Control.OutputCapable.Blocks

import Global


description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ input = do
  paragraph $ translate $ do
    german "Betrachten Sie die folgende Liste von Zahlen."
    english "Consider the following list of numbers."
  code $ show input
  paragraph $ translate $ do
    german "Wählen Sie zwei Elemente der Liste aus."
    english "Choose two elements of the list."
  paragraph $ translate $ do
    german "Die Antwort muss folgendes erfüllen:"
    english "Your answer has to satisfy the following:"
  itemizeM [
      translate $ do
       german "Keines hat zwei negative Nachbarn."
       english "None has two negative neighbours."
    , translate $ do
       german "Keines hat zwei durch 3 teilbare Nachbarn."
       english "None has two neighbours divisible by 3."
    ]
  pure ()

=============================================

module Parse (parseSubmission) where


import Control.OutputCapable.Blocks     (LangM', OutputCapable, ReportT)
import FlexTask.Generic.Parse (
  formParser,
  parseWithOrReport,
  reportWithFieldNumber,
  )

import Global




parseSubmission :: (Monad m, OutputCapable (ReportT o m)) => String -> LangM' (ReportT o m) Submission
parseSubmission = parseWithOrReport formParser reportWithFieldNumber
