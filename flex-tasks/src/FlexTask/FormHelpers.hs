{-# language TypeApplications #-}

module FlexTask.FormHelpers (
  labeledCheckboxes,
  ) where


import Yesod                            (FieldSettings)

import FlexTask.FormUtil                (universalLabel)
import FlexTask.Generic.Form (
  Alignment,
  MultipleChoiceSelection,
  buttons,
  formify,
  )
import FlexTask.YesodConfig             (FlexForm, Rendered, Widget)



{- |
A multiple choice checkbox field.
Checkboxes are labeled with their index number and the provided (monolingual) labels.
-}
labeledCheckboxes
  :: Alignment
  -- ^ vertical or horizontal
  -> FieldSettings FlexForm
  -- ^ FieldSettings of the header label: attributes, label text, etc.
  -> [String]
  -- ^ individual option labels
  -> Rendered Widget
labeledCheckboxes alignment fSettings labels = formify (Nothing @MultipleChoiceSelection)
  [[buttons
      alignment
      fSettings
      $ zipWith (\a b -> universalLabel $ show a ++ ". " ++ b) [1 :: Integer ..] labels
  ]]
