{-# language DeriveGeneric #-}

{-|
Premade data types for specific form needs.
-}

module FlexTask.InputTypes (
  MultipleChoice(..),
  Hidden(..),
  SingleInputList(..),
  SingleChoiceSelection,
  singleChoiceAnswer,
  singleChoiceEmpty,
  getAnswer,
  getAnswerAsIndex,
  MultipleChoiceSelection,
  multipleChoiceAnswer,
  multipleChoiceEmpty,
  getAnswers,
  getAnswersAsIndices,
  ) where


import Data.List.Extra                  (nubSort)
import GHC.Generics                     (Generic)
import Yesod                            (PathPiece(..))


{- $setup
>>> import FlexTask.Form.Util
>>> import FlexTask.Form.Core
>>> :set -XOverloadedStrings
-}


-- | Wrapper representing zero or more selected values in a multiple-choice field.
newtype MultipleChoice a = MultipleChoice
  { getChoices :: [a]
  -- ^ Retrieve the selected values
  } deriving (Eq, Generic, Show)


{- |
Wrapper type for generating hidden fields.
This can be used to transfer static information through the form to parsing.
Note that the generated field still has a label.
If the label is not left blank, then it will be displayed as normal.

__The user can inspect and modify the contents of a hidden field.__
__Do not use hidden fields for sensitive information or values whose integrity must be trusted.__

=== __Example__

>>> printWidget "en" $ formify (Just $ Hidden 3) $ basic ""
<div class="flex-form-div form-group">
...
    <label for="flexident1">
    </label>
    <input type="hidden" id="flexident1" ... value="3">
...
</div>
-}
newtype Hidden a = Hidden {getHidden :: a} deriving (Eq,Show)


instance PathPiece a => PathPiece (Hidden a) where
  fromPathPiece = fmap Hidden . fromPathPiece
  toPathPiece = toPathPiece . getHidden


{- |
Wrapper type for lists. Use for a single field list input.
Normally, lists are interpreted as multiple fields instead.

=== __Example__

>>> printWidget "en" $ formify @(SingleInputList String) Nothing $ basic "Input comma separated sentences"
<div class="flex-form-div form-group">
...
    <label for="flexident1">
      Input comma separated sentences
    </label>
    <input id="flexident1" ... type="text" ...>
...
</div>

Note that this does not actually enforce any kind of input syntax.
The generated input itself is a simple text field.
The comma separation is checked only when parsing with the matching `FlexTask.Generic.Parse.formParser`.
-}
newtype SingleInputList a = SingleInputList {getList :: [a]} deriving (Eq,Show)


{- |
Generic single choice answer type.
Use if both of the following are true:

  - You want an input that presents multiple answer choices, but only allows a single selection.
  - There's no specific data type associated with this selection.
-}
newtype SingleChoiceSelection = SingleChoiceSelection
  {getAnswer :: Int
  -- ^ Retrieve the selected option. (The first selectable option is @1@)
  } deriving (Show,Eq,Generic)


{- |
Same as `getAnswer` but the selections are counted from @0@ instead of from @1@.
Use if you want to pass the selected answer to an indexing function like `!!` or `Data.List.!?`.
-}
getAnswerAsIndex :: SingleChoiceSelection -> Int
getAnswerAsIndex = subtract 1 . getAnswer


{-# DEPRECATED singleChoiceEmpty
  "This function only existed to satisfy a legacy interface in Autotool. It will be removed in a future version."
  #-}
-- | Value with no option selected.
singleChoiceEmpty :: SingleChoiceSelection
singleChoiceEmpty = singleChoiceAnswer 0


-- | Value with given number option selected.
singleChoiceAnswer :: Int -> SingleChoiceSelection
singleChoiceAnswer = SingleChoiceSelection


{- |
Same as `SingleChoiceSelection`, but for multiple choice input.
Use if both of the following are true:

  - You want an input that presents multiple answer choices and allows selecting any number of them.
  - There's no specific data type associated with this selection.
-}
type MultipleChoiceSelection = MultipleChoice SingleChoiceSelection


{- |
Retrieve the list of selected options.
The first selectable option is @1@.
@[]@ if none are selected.
-}
getAnswers :: MultipleChoiceSelection -> [Int]
getAnswers = map getAnswer . getChoices


{- |
Same as `getAnswers` but the selections are counted from @0@ instead of from @1@.
Use if you want to pass the selected answers to an indexing function like `!!` or `Data.List.!?`.
-}
getAnswersAsIndices :: MultipleChoiceSelection -> [Int]
getAnswersAsIndices = map (subtract 1) . getAnswers


-- | Value with no options selected.
multipleChoiceEmpty :: MultipleChoiceSelection
multipleChoiceEmpty = MultipleChoice []


{- |
Value with given list of options selected.
Sorts the input list and removes duplicates.
-}
multipleChoiceAnswer :: [Int] -> MultipleChoiceSelection
multipleChoiceAnswer = MultipleChoice . map singleChoiceAnswer . nubSort
