{-# language TypeApplications #-}

{- |
Helpers for commonly used form patterns.
-}

module FlexTask.FormHelpers (
  anonymousRadioButtons,
  labeledRadioButtons,
  labeledCheckboxes,
  ) where


import Yesod                            (FieldSettings, SomeMessage)

import FlexTask.FormUtil                (showToUniversalLabel, universalLabel)
import FlexTask.Generic.Form (
  Alignment,
  MultipleChoiceSelection,
  SingleChoiceSelection,
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
labeledCheckboxes alignment fSettings labels = formify
  (Nothing @MultipleChoiceSelection)
  [[buttons
      alignment
      fSettings
      $ zipWith (\a b -> universalLabel $ show a ++ ". " ++ b) [1 :: Integer ..] labels
  ]]


{- |
A single choice radio button field.
Buttons are labeled with just their index number.
-}
anonymousRadioButtons
  :: Integral i
  => Alignment
  -- ^ vertical or horizontal
  -> FieldSettings FlexForm
  -- ^ heading label, attributes, etc.
  -> i
  -- ^ the amount of options to provide
  -> Rendered Widget
anonymousRadioButtons alignment fSettings amount = formify (Nothing @SingleChoiceSelection)
  [[buttons
    alignment
    fSettings
    $ map showToUniversalLabel [1.. toInteger amount]
  ]]


{- |
A single choice radio button field.
Buttons are labeled with the given multilingual labels.
-}
labeledRadioButtons
  :: Alignment
  -- ^ vertical or horizontal
  -> FieldSettings FlexForm
  -- ^ heading label, attributes, etc.
  -> [SomeMessage FlexForm]
  -- ^ individual option labels
  -> Rendered Widget
labeledRadioButtons alignment fSettings labels = formify
  (Nothing :: Maybe SingleChoiceSelection)
  [[buttons alignment fSettings labels]]
