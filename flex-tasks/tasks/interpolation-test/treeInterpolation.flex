{-# language DeriveDataTypeable #-}

module Global where


import Data.Data     (Data)

data Tree = Leaf Int | Branch (Maybe Tree) (Maybe Tree) Int deriving (Data,Eq,Show)


addToTree :: [Int] -> Tree
addToTree [] = error "no"
addToTree (x:xs) = foldl sortIn (Leaf x) xs


sortIn :: Tree -> Int -> Tree
sortIn (Leaf x) y
  | x > y = Branch Nothing (Just $ Leaf y) x
  | otherwise = Branch (Just $ Leaf y) Nothing x
sortIn (Branch t1 t2 x) y
  | x > y = case t2 of
              Nothing   -> Branch t1 (Just $ Leaf y) x
              Just tree -> Branch t1 (Just $ sortIn tree y) x
  | otherwise = case t1 of
                  Nothing   -> Branch (Just $ Leaf y) t2 x
                  Just tree -> Branch (Just $ sortIn tree y) t2 x


getSum :: Tree -> Int
getSum (Leaf x) = x
getSum (Branch t1 t2 x) = x + maybe 0 getSum t1 + maybe 0 getSum t2


getProd :: Tree -> Int
getProd (Leaf x) = x
getProd (Branch t1 t2 x) = x * maybe 1 getProd t1 * maybe 1 getProd t2


type Submission = (Int,Int)
type TaskData = Tree

=============================================

module TaskSettings where


import Control.OutputCapable.Blocks     (LangM, OutputCapable)


validateSettings :: OutputCapable m => LangM m
validateSettings = pure ()

=============================================

{-# language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module TaskData (getTask) where


import Control.Monad.Random    (MonadRandom)
import Data.String.Interpolate (i)
import Data.Text               (Text)
import FlexTask.FormUtil       (getFormData)
import FlexTask.GenUtil        (fromGen)
import FlexTask.Generic.Form
import FlexTask.Types          (HtmlDict)
import FlexTask.YesodConfig    (Rendered, Widget)
import Test.QuickCheck.Gen


import Global




getTask :: MonadRandom m => m (TaskData, String, IO ([[Text]],HtmlDict))
getTask = fromGen $ do
    tree <- addToTree <$> vectorOf 100 (chooseInt (-100000,100000))
    pure (tree, checkers tree, getFormData form)



fields :: [[FieldInfo]]
fields = [[single "Tree Sum"],[single "Tree Product"]]


form :: Rendered Widget
form = formify (Nothing :: Maybe Submission) fields


checkers :: Tree -> String
checkers solution = [i|

{-\# language ApplicativeDo \#-}

module Check (checkSemantics, checkSyntax) where


import Control.OutputCapable.Blocks

import Global


checkSyntax :: OutputCapable m => FilePath -> a -> Submission -> LangM m
checkSyntax _ _ _ = pure ()


checkSemantics :: OutputCapable m => FilePath -> a -> Submission -> Rated m
checkSemantics _ _ (s,p) = do
    assertion (s == #{getSum solution}) $
      translate $ do
        german "Summe ist korrekt?"
        english "Sum ist correct?"
    assertion (p == #{getProd solution}) $ do
      translate $ do
        german "Produkt ist korrekt?"
        english "Product is correct?"
    pure 1.0
|]

=============================================

{-# Language ApplicativeDo #-}

module Description (description) where


import Control.OutputCapable.Blocks

import Global



description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ input = do
  code $ show input
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
