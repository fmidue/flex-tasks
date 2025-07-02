{-# language DeriveDataTypeable #-}

module Global where


import Data.Data (Data)
import LogicTasks.Formula (TruthValue(..))
import Trees.Types (PropFormula)


newtype Table = Table [(Maybe (PropFormula Char), [Maybe TruthValue])] deriving (Eq,Show)

data Namen = A | B | C | D deriving (Data,Eq,Enum,Bounded,Show)

type FormType = (String,[Namen])

type TaskData = (String,[String],[Namen])
type Submission = (Table,PropFormula Char,[Namen])



============================================

module TaskSettings where

-- 2024: Weight 1.0 (in Logik, Task13)

import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  indent,
  refuse,
  text,
  )

import Global                           (Namen)


emptyColumns, staticColumns, totalColumns, rows :: Int
emptyColumns  = 13
staticColumns = length [minBound .. maxBound :: Namen]
totalColumns  = staticColumns + emptyColumns
rows          = 2^staticColumns

tableRequired, showSolution :: Bool
tableRequired = False
showSolution  = True


validateSettings :: OutputCapable m => LangM m
validateSettings
  | emptyColumns < 5 = refuse $ indent $ text
      "Die Anzahl der leeren Spalten ist kleiner als die Anzahl der Hinweise."
  | totalColumns > 18 = refuse $ indent $ text $
      "Die Tabelle enthält zu viele Spalten. " ++
      "Das Eingabeformular kann bei über 18 Spalten nicht mehr korrekt angezeigt werden."
  | otherwise = pure ()

=============================================

{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module TaskData (getTask) where


import Data.Char               (digitToInt)
import Data.Foldable           (toList)
import Data.List               (transpose)
import Data.Maybe              (fromJust)
import Data.Text               (Text)
import Data.String.Interpolate (i)
import FlexTask.FormUtil (
  ($$>),
  addCss,
  addCssClass,
  getFormData,
  universalLabel,
  )
import FlexTask.Generic.Form (
  Alignment(..),
  FieldInfo,
  Formify(..),
  formify,
  formifyInstanceMultiChoice,
  single,
  buttonsEnum
  )
import FlexTask.Types          (HtmlDict)
import FlexTask.YesodConfig    (Rendered, Widget)
import LogicTasks.Forms        (tableForm)
import LogicTasks.Formula      (TruthValue(..))
import Numeric                 (showBin)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen
import Yesod

import Trees.Types (BinOp(..), SynTree(..))
import Trees.Print (simplestDisplay)

import Global
import TaskSettings



instance MonadFail Gen where
  fail = error


formulaClass :: Text
formulaClass = "formula"


getParticipants :: Gen [Bool]
getParticipants = vectorOf 4 arbitrary `suchThat` \bs -> or bs && not (and bs)


getTask :: Gen (TaskData, String, IO ([Text],HtmlDict))
getTask = do
    [a,b,c,d] <- getParticipants
    names <- getNames
    (formula, legend, hints) <- formulaAndHints a b c d names
    let
      zipped = [(a, A), (b, B), (c, C), (d, D)]
      coming = [ n | (True,n) <- zipped ]
    pure ((legend, hints, coming), checkers formula, getFormData (form names))
  where
    form :: (String,String,String,String) -> Rendered Widget
    form n = addCss formulaCss $
      tableForm emptyColumns rows ["A","B","C","D"] [] $$>
      formify (Nothing :: Maybe FormType) (nonTableFields n)

    getNames :: Gen (String,String,String,String)
    getNames = do
      a <- elements ["Astrid","Anna"]
      b <- elements ["Ben","Bernd"]
      c <- elements ["Claudia","Carla"]
      d <- elements ["David","Daniel"]
      return (a,b,c,d)

    formulaCss = [cassius|
      .flex-form-span > label
        display:block
        margin-bottom: 1em
        margin-top: 2em

      .#{formulaClass}
        display:block
        width: 95%
        margin-left: auto
        margin-right: auto
        margin-bottom: 0.5em
    |]

formulaAndHints :: Bool -> Bool -> Bool -> Bool -> (String,String,String,String) -> Gen (SynTree BinOp Char, String, [String])
formulaAndHints a b c d (aN,bN,cN,dN) = do
    shuffled <- shuffle (zip [part1,part2,part3,part4,part5] [hint1,hint2,hint3,hint4,hint5])
    let (parts,hints) = unzip shuffled
    let formula = foldr1 (Binary And) parts
    pure (formula, namesLegend, hints)
  where
    namesLegend = [i|Sie fragt ihre Freunde #{aN} (A), #{bN} (B), #{cN} (C) und #{dN} (D), |]
    hint1 = (if b == d
              then [i|Falls #{bN} und #{dN}#{nicht b} kommen,|]
              else
                if b
                  then [i|Falls #{bN} nicht, aber #{dN} kommt,|]
                  else [i|Falls #{bN}, aber nicht #{dN} kommt,|]
            )
            ++ [i| kommt #{cN}#{nicht (not c)}.|]
    hint2 = [i|#{bN} kommt#{nicht b} mit, wenn #{aN}#{nicht a} mitkommt.|]
    hint3 = [i|Wenn #{bN}#{nicht b} kommt, kommt #{if b == c then auch else ""} #{cN}#{nicht c}.|]
    hint4 = [i|Wenn #{cN}#{nicht c} kommt, kommt #{if c == d then auch else ""} #{dN}#{nicht d}.|]
    hint5 = [i|Wenn #{dN}#{nicht d} kommt, |]
         ++ if a == b
              then [i|kommen #{if d == a then auch else ""} #{aN} oder #{bN}#{nicht a}.|]
              else
                if a
                  then [i|kommt #{if d then auch else ""} #{aN} nicht oder #{bN} kommt.|]
                  else [i|kommt #{if d then "" else auch} #{aN} oder #{bN} kommt nicht.|]

    nicht :: Bool -> String
    nicht f = if f then " nicht" else ""

    -- to avoid inline type annotations due to String vs Text
    auch :: String
    auch = " auch"

    neg expr x = if expr then Not (Leaf x) else Leaf x
    part1 = Binary Impl
             (Binary And (neg b 'B') (neg d 'D'))
             (neg (not c) 'C')
    part2 = Binary Impl (neg a 'A') (neg b 'B')
    part3 = Binary Impl (neg b 'B') (neg c 'C')
    part4 = Binary Impl (neg c 'C') (neg d 'D')
    part5 = Binary Impl
             (neg d 'D')
             (Binary Or (neg a 'A') (neg b 'B'))



nonTableFields :: (String,String,String,String) -> [[FieldInfo]]
nonTableFields (a,b,c,d) = [
      [single $ addCssClass formulaClass "Formel"]
    , [buttonsEnum Vertical "Wer kommt?" getName]
    ]
  where
    nameMatching = [(A, a), (B, b), (C, c), (D, d)]
    getName = universalLabel . fromJust . flip lookup nameMatching



instance Formify [Namen] where
  formifyImplementation = formifyInstanceMultiChoice



startingTable :: [[Maybe TruthValue]]
startingTable = map (Just . TruthValue . toEnum . digitToInt) <$>
    transpose (pad . (`showBin` "") <$> [0..rows -1])
  where pad s = replicate (staticColumns - length s) '0' ++ s



checkers :: SynTree BinOp Char -> String
checkers fSol = [i|

{-\# language ApplicativeDo \#-}

module Check (checkSemantics, checkSyntax) where


import Control.Monad                    (when)
import Control.OutputCapable.Blocks
import Data.Containers.ListUtils        (nubOrd)
import Data.Foldable                    (toList)
import Data.List (
  delete,
  isInfixOf,
  transpose,
  )
import Data.Maybe                       (catMaybes)
import Data.Map                         (fromList)
import Data.Ratio                       ((%))

import LogicTasks.Formula (
  TruthValue(..),
  isSemanticEqual,
  convert,
  )
import Trees.Types (
  SynTree(..),
  PropFormula(..),
  BinOp(..),
  toSynTree
  )
import Trees.Print                      (simplestDisplay)

import qualified SAT.MiniSat            as Sat

import Global



correctColumn :: (PropFormula Char,[Maybe TruthValue]) -> Bool
correctColumn (f,bs) = all lookFor allocationsAndValues
  where
    ones = Sat.solve_all $ convert f
    allocations = filter ((`elem` atoms) . fst) <$>
      #{map (zip "ABCD" . map (truth . fromJust)) $ transpose startingTable} --ignore-length
    allocationsAndValues = [(truth a,b) | (Just a,b) <- zip bs allocations]
    lookFor (True,a) = fromList a `elem` ones
    lookFor (False,a) = fromList a `notElem` ones
    atoms = toList f



isSubFormula :: PropFormula Char -> PropFormula Char -> Bool
isSubFormula a b = noSpaces a `isInfixOf` noSpaces b
  where
    noSpaces = filter (/=' ') . show


dropStatic :: [a] -> [a]
dropStatic = drop #{staticColumns}


startingTable :: [[Maybe TruthValue]]
startingTable = #{startingTable} --ignore-length


checkSyntax :: OutputCapable m => a -> b -> Submission -> LangM m
checkSyntax _ _ (Table xs,f,n) = do
    paragraph $ do
      text "Es wurden Formeln wie folgt gelesen:"
      when (any (/=Nothing) mColumns) $ indent $ do
        text "Tabellenspalten"
        code $ unlines $ map (simplestDisplay . toSynTree) entered
        pure ()
      indent $ do
        text "Antwortformel"
        code $ simplestDisplay $ toSynTree f
        pure ()
      pure ()
    when (atomicColumns == map reverse startingTable) $ refuse $ indent $ text $
      "Die Spalten der atomaren Formeln sind invertiert. " ++
      "Bitte legen Sie die Tafel so an wie in der Vorlesung vorgegeben."
    assertion (nubOrd atomicRows == atomicRows) $ text
      "Alle vollständig eingetragenen Belegungen kommen nur einmal in Tafel vor?"
    assertion (notElem 'E' $ concatMap toList $ f : entered) $ text
      "Eva (E) kommt nicht in Formeln vor?"
    assertion (
      all (`elem` ('F': solutionAtoms)) (concatMap toList entered) &&
      all (`elem` solutionAtoms) (toList f)
      )
      $ text "Es wurden nur bekannte atomare Formeln genutzt?"
    assertion (all noDuplicate entered) $ text
      "Tabellenspalten existieren nur einmal?"
    assertion (not (null n)) $ text
      "Es wurde mindestens ein Name angekreuzt? (Alleine gehen ist ausgeschlossen.)"
    pure ()
  where
    fs = map fst xs
    atomicColumns = take #{staticColumns} $ map snd xs
    mColumns = dropStatic fs
    entered = catMaybes mColumns
    noDuplicate formula = formula `notElem` (map Atomic "ABCD" ++ delete formula entered)
    atomicRows = filter (notElem Nothing) $ transpose atomicColumns
    solutionAtoms = #{show $ toList fSol} --ignore-length


checkSemantics :: OutputCapable m => a -> (b,c,[Namen]) -> Submission -> Rated m
checkSemantics _ (_,_,nSol) (Table xs,f,n) = do
    let correctStart = take #{staticColumns} columns == startingTable
    #{checkType} correctStart $ text
      "Spalten der atomaren Formeln ergeben sinnvolle Wahrheitstafel?"
    let subFormulas = all (\\e -> e `isSubFormula` f || e `elem` negations) $ catMaybes $ dropStatic replaceFAll
    #{checkType} subFormulas $ text
      "Als Tabellenspalten kommen nur Teilformeln der Gesamtformel vor?"
    let correctValues = all correctColumn zippedEntries && not (all (null . catMaybes . snd) zippedEntries)
    #{checkType} correctValues $ text
      "Tafel ist gefüllt und Tabellenspalten enthalten nur korrekt ermittelte Wahrheitswerte?"
    yesNo (all (`elem` f) "ABCD") $ text
       "Gesamtformel enthält alle vorkommenden atomaren Formeln?"
    let correctFormula = isSemanticEqual (#{fSol}) (toSynTree f) --ignore-length
    yesNo correctFormula $ text "Gesamtformel ist korrekt?"
    let correctNames = n == nSol
    yesNo correctNames $ text "Die Auflistung der Begleitenden ist korrekt?"
    let correct = filter id [correctStart, correctFormula, correctNames, correctValues]
    let points = fromIntegral (length correct) % 4
    res <- printSolutionAndAssertMinimum (MinimumThreshold (1 % 4)) IndefiniteArticle maybeAnswer points
    pure res
  where
    (headers,columns) = unzip xs
    maybeAnswer =
      flip (++) (show nSol) <$>
        #{if showSolution
            then Just ("Formel: " ++ simplestDisplay fSol ++ "\nKorrekte Einträge in Wahrheitstafel.\nBegleitende: ")
            else Nothing
         } --ignore-length
    replaceF (Just (Atomic 'F')) = Just f
    replaceF a                   = a

    replaceFAll = map replaceF headers
    zippedEntries = [(sf,b)| (Just sf,b) <- dropStatic $ zip replaceFAll columns]
    negations = map (Neg . Atomic) ['A'..'D']


|]
  where
    checkType :: String
    checkType = if tableRequired then "assertion" else "yesNo"

=============================================

{-# Language ApplicativeDo #-}

module Description (description) where


import Control.OutputCapable.Blocks
import LogicTasks.Keys                  (keyHeading, basicOpKey, arrowsKey)



description :: OutputCapable m => a -> (String,[String],b) -> LangM m
description _ (legend,hints,_) = do
    paragraph $ text
      ( "Eva möchte gerne auf ein Konzert ihrer Lieblingsband gehen. " ++
        legend ++
        "ob sie mitkommen wollen. Leider sind die Antworten etwas seltsam."
      )
    itemizeM $ map text hints
    paragraph $ text $
      "Übersetzen Sie die Kombination der Antworten in eine aussagenlogische Formel. " ++
      "Geben Sie diese Formel in das entsprechend benannte Textfeld ein. " ++
      "Verwenden Sie dabei die atomaren Formeln A, B, C, D mit der Interpretation, " ++
      "dass eine Zuordnung von 'wahr' dafür steht, dass die entsprechende Person mitkommt."
    keyHeading
    basicOpKey True
    arrowsKey
    paragraph $ text $
      "Wer geht mit Eva zum Konzert? Leiten Sie Ihr Ergebnis mittels Wahrheitstafel her. " ++
      "Kreuzen Sie dann alle Begleitenden in der Namensliste an."
    paragraph $ text $
      "Beim Ausfüllen der Wahrheitstafel können Spalten leer gelassen werden. " ++
      "Sollten Sie für die Eingabe einer Formelüberschrift mehr Platz benötigen, " ++
      "können Sie die kleineren Eingabefelder überspringen und eines der größeren nutzen."
    pure ()


=============================================

module Parse (parseSubmission) where


import Control.OutputCapable.Blocks (LangM', OutputCapable, ReportT)
import Control.OutputCapable.Blocks.Generic (($>>=))
import Data.List.Extra        (chunksOf, transpose)
import FlexTask.Generic.Parse (
  Parse(..),
  parseInstanceMultiChoice,
  displayInputAnd,
  escaped,
  parseWithFallback,
  parseWithOrReport,
  reportWithFieldNumber,
  )
import Formula.Parsing.Delayed (
  complainAboutMissingParenthesesIfNotFailingOn,
  )
import LogicTasks.Formula      (TruthValue)
import LogicTasks.Parsing      (formulaSymbolParser, parser)
import ParsingHelpers          (fully)
import Trees.Types             (PropFormula(Atomic))

import Global
import TaskSettings


instance Parse TruthValue where
  formParser = escaped parser


instance Parse [Namen] where
  formParser = parseInstanceMultiChoice


makeTable :: [Maybe (PropFormula Char)] -> [Maybe TruthValue] -> Table
makeTable headers values = Table $ zip allHeaders formattedTruthValues
  where
    allHeaders = map (Just . Atomic) "ABCD" ++ headers
    formattedTruthValues = transpose $ chunksOf totalColumns values


parseSubmission :: (Monad m, OutputCapable (ReportT o m)) => String -> LangM' (ReportT o m) Submission
parseSubmission input =
    parseWithOrReport formParser reportWithFieldNumber input $>>= \(headerStrings,columns,formulaString,names) ->
      traverse (traverse parseIt) headerStrings $>>= \parsedHeaders ->
        parseIt formulaString $>>= \parsedFormula ->
          pure (makeTable parsedHeaders columns, parsedFormula, names)
  where
    parseIt =
      parseWithFallback
        (fully parser)
        (displayInputAnd complainAboutMissingParenthesesIfNotFailingOn)
        (fully formulaSymbolParser)
