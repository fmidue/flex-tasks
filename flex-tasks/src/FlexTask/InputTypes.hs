{-# Language DeriveGeneric #-}

{- |
Data types for use in input forms and related functions.
-}

module FlexTask.InputTypes (
  -- * Data Types
  SingleChoiceSelection,
  MultipleChoiceSelection,
  Hidden(..),
  SingleInputList(..),

  -- * Anonymous Enum Type Builders and Accessors
  getAnswer,
  getAnswerAsIndex,
  getAnswers,
  getAnswersAsIndices,
  multipleChoiceAnswer,
  multipleChoiceEmpty,
  singleChoiceAnswer,
  singleChoiceEmpty,
  ) where


import Data.List.Extra                  (nubSort)
import GHC.Generics                     (Generic)


{- $setup
>>> import FlexTask.Form.Util
>>> import FlexTask.Form.Internal
-}


{- |
Wrapper type for generating hidden fields.
This can be used to transfer static information through the form to parsing.
Note that the generated field still has a label.
If the label is not left blank, then it will be displayed as normal.

=== __Example__

>>> printWidget "en" $ formify (Just $ Hidden 3) [[single ""]]
<div class="flex-form-div form-group">
...
    <label for="flexident1">
    </label>
    <input type="hidden" id="flexident1" ... value="3">
...
</div>
-}
newtype Hidden a = Hidden {getHidden :: a} deriving (Eq,Show)


{- |
Wrapper type for lists. Use for a single field list input.
Normally, lists are interpreted as multiple fields instead.

=== __Example__

>>> printWidget "en" $ formify (Nothing @(SingleInputList String)) [[single "Input comma separated sentences"]]
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
Use if both of the following is true:
  - You want an input that presents multiple answer choices, but only allows a single selection.
  - There's no specific data type associated with this selection.

=== __Example__

>>> let labels = ["First Option", "Second Option", "Third Option"]
>>> printWidget "en" $ formify (Just $ singleChoiceAnswer 3) [[dropdown "Choose one" labels]]
<div class="flex-form-div form-group">
...
    <label for="flexident1">
      Choose one
    </label>
    <select id="flexident1" ...>
      <option value="" selected disabled>
        &lt;None&gt;
      </option>
      <option value="1">
        First Option
      </option>
      <option value="2">
        Second Option
      </option>
      <option value="3" selected>
        Third Option
      </option>
    </select>
...
</div>
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


{- |
Same as `SingleChoiceSelection`, but for multiple choice input.
Use if both of the following is true:
  - You want an input that presents multiple answer choices and allows selecting any number of them.
  - There's no specific data type associated with this selection.

=== __Example__

>>> let labels = ["First Option", "Second Option", "Third Option"]
>>> printWidget "en" $ formify (Just $ multipleChoiceAnswer [1,2]) [[dropdown "Choose one" labels]]
<div class="flex-form-div form-group">
...
    <label for="flexident1">
      Choose one
    </label>
    <select id="flexident1" ... multiple>
      <option value="1" selected>
        First Option
      </option>
      <option value="2" selected>
        Second Option
      </option>
      <option value="3">
        Third Option
      </option>
    </select>
...
</div>
-}
newtype MultipleChoiceSelection = MultipleChoiceSelection
  { getAnswers :: [Int]
  -- ^ Retrieve the list of selected options. The first selectable option is @1@ . @[]@ if none are selected.
  } deriving (Show,Eq,Generic)


{- |
Same as `getAnswers` but the selections are counted from @0@ instead of from @1@.
Use if you want to pass the selected answers to an indexing function like `!!` or `Data.List.!?`.
-}
getAnswersAsIndices :: MultipleChoiceSelection -> [Int]
getAnswersAsIndices = map (subtract 1) . getAnswers


{-# DEPRECATED singleChoiceEmpty
  "This function only existed to satisfy a legacy interface in Autotool. It will be removed in a future version."
  #-}
-- | Value with no option selected.
singleChoiceEmpty :: SingleChoiceSelection
singleChoiceEmpty = singleChoiceAnswer 0


-- | Value with given number option selected.
singleChoiceAnswer :: Int -> SingleChoiceSelection
singleChoiceAnswer = SingleChoiceSelection


-- | Value with no options selected.
multipleChoiceEmpty :: MultipleChoiceSelection
multipleChoiceEmpty = MultipleChoiceSelection []


{- |
Value with given list of options selected.
The order of list elements is inconsequential.
-}
multipleChoiceAnswer :: [Int] -> MultipleChoiceSelection
multipleChoiceAnswer = MultipleChoiceSelection . nubSort
