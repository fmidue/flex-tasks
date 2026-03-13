{-# language TypeApplications #-}

module FlexTask.FormHelpers (
  labeledCheckboxes,
  ) where


import Yesod

import FlexTask.FormUtil
import FlexTask.Generic.Form
import FlexTask.YesodConfig



labeledCheckboxes :: Alignment -> FieldSettings FlexForm -> [[Char]] -> Rendered Widget
labeledCheckboxes alignment fSettings labels = formify (Nothing @MultipleChoiceSelection)
  [[buttons
      alignment
      fSettings
      $ zipWith (\a b -> universalLabel $ show a ++ ". " ++ b) [1 :: Int ..] labels
  ]]
