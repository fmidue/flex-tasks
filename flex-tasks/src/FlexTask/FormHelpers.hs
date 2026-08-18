
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
  ChoiceShape(..),
  MultipleChoiceSelection,
  SimpleFormPiece,
  SingleChoiceSelection,
  multipleChoice,
  single,
  singleChoice,
  )
import FlexTask.YesodConfig             (FlexForm)



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
  -> SimpleFormPiece t MultipleChoiceSelection
labeledCheckboxes alignment fSettings = single . multipleChoice
  (Buttons alignment)
  fSettings
  . zipWith (\a b -> universalLabel $ show a ++ ". " ++ b) [1 :: Integer ..]


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
  -> SimpleFormPiece t SingleChoiceSelection
anonymousRadioButtons alignment fSettings amount = single $ singleChoice
  (Buttons alignment)
  fSettings
  $ map showToUniversalLabel [1.. toInteger amount]


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
  -> SimpleFormPiece t SingleChoiceSelection
labeledRadioButtons alignment fSettings =
  single . singleChoice (Buttons alignment) fSettings
