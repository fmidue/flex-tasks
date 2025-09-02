{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# language DefaultSignatures #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Generic.Form
  (
    -- * Data Types
    Alignment(..)
  , FieldInfo
  , SingleChoiceSelection
  , MultipleChoiceSelection
  , Hidden(..)
  , SingleInputList(..)
    -- * Type Classes
  , BaseForm(..)
  , Formify(..)
  , formify
  , formifyComponents
  , formifyComponentsFlat
    -- * Anonymous Enum Type Builders and Accessors.
  , getAnswer
  , getAnswers
  , multipleChoiceAnswer
  , multipleChoiceEmpty
  , singleChoiceAnswer
  , singleChoiceEmpty

    -- * Field Builders
  , buttons
  , buttonsEnum
  , dropdown
  , dropdownEnum
  , list
  , listWithoutLabels
  , single

    -- * Formify Convenience Functions
  , formifyInstanceBasicField
  , formifyInstanceOptionalField
  , formifyInstanceSingleChoice
  , formifyInstanceMultiChoice
  ) where


import Data.Data            (Data)
import Data.List.Extra      (intercalate, nubSort, uncons, unsnoc)
import Data.Tuple.Extra     (first)
import Data.Maybe           (fromMaybe)
import GHC.Generics         (Generic(..), K1(..), M1(..), (:*:)(..))
import GHC.Utils.Misc       (equalLength)
import Data.Text            (Text, pack, unpack)
import Yesod

import FlexTask.Widgets
  ( checkboxField
  , horizontalRadioField
  , joinRenders
  , renderForm
  )
import FlexTask.YesodConfig (FlexForm(..), Handler, Rendered, Widget)


{- $setup
>>> :set -XTypeApplications
>>> import FlexTask.FormUtil
>>> data MyType = One | Two | Three deriving (Bounded, Enum, Eq, Show)
>>> newtype MyCoolType = CType { getString :: String}
>>> let toCool = CType
>>> let fromCool = getString
>>> let basisField = baseForm
-}


{- |
Data type representing a prebuilt input field.
This type is used to determine the structure of a generated form.
The form is represented by a @[[FieldInfo]]@ type value.
Each FieldInfo value is an individual form element.
Inner lists represent the rows of the form.
All FieldInfo values in an inner list are rendered besides each other.
Inner lists are rendered below each other.

__Examples__

Input

@
[[single \"field1\", single \"field2\"]]
@

Renders as:

@
field1     field2
@

Input

@
[[single \"field1\"], [single \"field2\"]]
@

Renders as:

@
field1

field2
@

__Caution: Not all horizontal alignments work as one would expect.__
__If an element uses inner `Alignment` parameters,__
__then the next form will only be rendered besides the last form component of the former.__

Input

@
[[listWithoutLabels Vertical 2 []],[listWithoutLabels Vertical 2 []]]
@

will __not__ result in

@
list11      list21

list12      list22
@

but instead in

@
list11

list12     list21

list22
@
-}
data FieldInfo
  = Single (FieldSettings FlexForm)
  | List Alignment [FieldSettings FlexForm]
  | ChoicesDropdown (FieldSettings FlexForm) [SomeMessage FlexForm]
  | ChoicesButtons Alignment (FieldSettings FlexForm) [SomeMessage FlexForm]
  | InternalListElem (FieldSettings FlexForm)
  deriving (Show)


-- For tests; TODO: Move completely into test suite
deriving instance Show (FieldSettings FlexForm)

instance Show (SomeMessage FlexForm) where
  show m = '(': intercalate ", "
      [ "German: " <> inLang "de"
      , "English: " <> inLang "en"
      ]
      ++ ")"
    where
      inLang l = show $ renderMessage FlexForm{} [l] m


-- | Inner alignment of input field elements.
data Alignment = Horizontal | Vertical deriving (Eq,Show)


{- |
Wrapper type for generating hidden fields.

This can be used to transfer static information through the form to parsing.
Note that the generated field still has a label.
If the label is not left blank, then it will be displayed as normal.

=== __Example__

>>> printWidget "en" $ formify (Just $ Hidden 3) [[single ""]]
<div class="flex-form-div">
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
<div class="flex-form-div">
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
<div class="flex-form-div">
...
    <label for="flexident1">
      Choose one
    </label>
    <select id="flexident1" ...>
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
  {getAnswer :: Maybe Int -- ^ Retrieve the selected option. @Nothing@ if none.
  } deriving (Show,Eq,Generic,Data)
{- |
Same as `SingleChoiceSelection`, but for multiple choice input.

Use if both of the following is true:

  - You want an input that presents multiple answer choices and allows selecting any number of them.
  - There's no specific data type associated with this selection.

=== __Example__

>>> let labels = ["First Option", "Second Option", "Third Option"]
>>> printWidget "en" $ formify (Just $ multipleChoiceAnswer [1,2]) [[dropdown "Choose one" labels]]
<div class="flex-form-div">
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
  { getAnswers :: [Int] -- ^ Retrieve the list of selected options. @[]@ if none.
  } deriving (Show,Eq,Generic,Data)


-- | Value with no option selected.
singleChoiceEmpty :: SingleChoiceSelection
singleChoiceEmpty = SingleChoiceSelection Nothing


-- | Value with given number option selected.
singleChoiceAnswer :: Int -> SingleChoiceSelection
singleChoiceAnswer = SingleChoiceSelection . Just


-- | Value with no options selected.
multipleChoiceEmpty :: MultipleChoiceSelection
multipleChoiceEmpty = MultipleChoiceSelection []

{- |
Value with given list of options selected.
The order of list elements is inconsequential.
-}
multipleChoiceAnswer :: [Int] -> MultipleChoiceSelection
multipleChoiceAnswer = MultipleChoiceSelection . nubSort



{- |
Members have a basic Yesod field representing Html input fields.
A `BaseForm` instance of type @a@ is needed for generically producing forms
for @[a]@ and @Maybe a@ types.
An instance can be given manually with the `Field` constructor
or using the `convertField` function on an existing `Field`.

=== __Example__

>>> instance BaseForm MyCoolType where baseForm = convertField toCool fromCool basisField
-}
class BaseForm a where
  baseForm :: Field Handler a


instance BaseForm Integer where
  baseForm = intField

instance BaseForm Int where
  baseForm = intField

instance BaseForm Text where
  baseForm = textField


instance BaseForm String where
  baseForm = convertField unpack pack textField


instance BaseForm Textarea where
  baseForm = textareaField


instance BaseForm Bool where
  baseForm = boolField


instance BaseForm Double where
  baseForm = doubleField


instance PathPiece a => PathPiece (Hidden a) where
  fromPathPiece = fmap Hidden . fromPathPiece
  toPathPiece = toPathPiece . getHidden


instance PathPiece a => BaseForm (Hidden a) where
  baseForm = hiddenField


-- This indicates I should probably change this class to something more succinct.
-- The first function is never used, since it normally handles the parsing.
instance Show a => BaseForm (SingleInputList a) where
  baseForm = convertField undefined (pack . intercalate ", " . map show . getList) textField


{- |
Class for generic generation of Html input forms for a given type.
Bodyless instances can be declared for any type instancing Generic.
__Exception: Types with multiple constructors.__
Use utility functions for those or provide your own instance.
-}
class Formify a where
  {- |
  __Direct use of this function is not recommended__
  __due to possible undetected invalidity of the result.__
  It should only be used when writing manual instances of `Formify`.
  Use `formify` or its variants instead.
  -}
  formifyImplementation
      :: Maybe a -- ^ Optional default value for form.
      -> [[FieldInfo]] -- ^ Structure and type of form.
      -> ([[FieldInfo]], [[Rendered Widget]]) -- ^ remaining form structure and completed sub-renders.

  default formifyImplementation
      :: (Generic a, GFormify (Rep a))
      => Maybe a
      -> [[FieldInfo]]
      -> ([[FieldInfo]], [[Rendered Widget]])
  formifyImplementation mDefault = gformify $ from <$> mDefault



class GFormify f where
  gformify :: Maybe (f a) -> [[FieldInfo]] -> ([[FieldInfo]], [[Rendered Widget]])



instance (GFormify a, GFormify b) => GFormify (a :*: b) where
  gformify mDefault xs = (rightRest, renders)
    where
      (left,right) = case mDefault of
        Nothing        -> (Nothing,Nothing)
        Just (a :*: b) -> (Just a, Just b)
      (leftRest, leftRender) = gformify left xs
      (rightRest, rightRender) = gformify right rightFieldInfo
      (rightFieldInfo,renders) = case leftRest of
        ([]:xss) -> (xss, leftRender ++ rightRender)
        rest   -> (rest, ls ++ [lastRenderLeft ++ firstRenderRight] ++ rs)
      (ls,lastRenderLeft) = fromMaybe (leftRender,[]) $ unsnoc leftRender
      (firstRenderRight,rs) = fromMaybe ([],rightRender) $ uncons rightRender



instance GFormify a => GFormify (M1 i c a) where
  gformify mDefault = gformify $ unM1 <$> mDefault



instance Formify a => GFormify (K1 i a) where
  gformify mDefault = formifyImplementation $ unK1 <$> mDefault


instance Formify Integer where
  formifyImplementation = formifyInstanceBasicField

instance Formify Int where
  formifyImplementation = formifyInstanceBasicField

instance Formify Text where
  formifyImplementation = formifyInstanceBasicField


instance Formify String where
  formifyImplementation = formifyInstanceBasicField


instance Formify Textarea where
  formifyImplementation = formifyInstanceBasicField


instance Formify Bool where
  formifyImplementation = formifyInstanceBasicField



instance Formify Double where
  formifyImplementation = formifyInstanceBasicField


instance PathPiece a => Formify (Hidden a) where
  formifyImplementation = formifyInstanceBasicField


instance Show a => Formify (SingleInputList a) where
  formifyImplementation = formifyInstanceBasicField


instance (Formify a, Formify b) => Formify (a,b)

instance (Formify a, Formify b, Formify c) => Formify (a,b,c)

instance (Formify a, Formify b, Formify c, Formify d) => Formify (a,b,c,d)

instance (Formify a, Formify b, Formify c, Formify d, Formify e) => Formify (a,b,c,d,e)

instance (Formify a, Formify b, Formify c, Formify d, Formify e, Formify f) => Formify (a,b,c,d,e,f)


instance {-# Overlappable #-} (BaseForm a, Formify a) => Formify [a] where
  formifyImplementation = formifyInstanceList



instance (BaseForm a, Formify a) => Formify (Maybe a) where
  formifyImplementation = formifyInstanceOptionalField


instance Formify (Maybe a) => Formify [Maybe a] where
  formifyImplementation = formifyInstanceList


instance Formify SingleChoiceSelection where
  formifyImplementation = renderNextSingleChoiceField (`zip` [1..]) . (=<<) getAnswer


instance Formify MultipleChoiceSelection where
  formifyImplementation = renderNextMultipleChoiceField (`zip` [1..]) . fmap getAnswers



{- |
This is the main way to build generic forms.
Use in conjunction with `FieldInfo` builders to generate a form.

Will fail if remaining `FieldInfo` structure is not empty,
indicating the form is faulty.


=== __Examples__

Renders an input field with /type=number/ attribute, no default value and label /Age/.

>>> printWidget "en" $ formify (Nothing @Int) [[single "Age"]]
<div class="flex-form-div">
...
    <label for="flexident1">
      Age
    </label>
    <input id="flexident1" name="flex1" type="number" step="1" required="" value="">
...
</div>

Renders a series of four input fields, each for the type String
and organized vertically beneath each other.
They are prefilled with the values given above,
are assigned the Css class \"helloInput\" and have no labels attached to them.

>>> let defaults = ["Hallo", "Hello", "Hola", "Ciao"]
>>> printWidget "en" $ formify (Just defaults) [[listWithoutLabels Vertical 4 [("class","helloInput")]]]
<div class="flex-form-div">
...
    <input id="flexident1" name="flex1" type="text" ... value="Hallo" class="helloInput">
...
</div>
<div class="flex-form-div">
...
    <input id="flexident2" name="flex1" type="text" ... value="Hello" class="helloInput">
...
</div>
<div class="flex-form-div">
...
    <input id="flexident3" name="flex1" type="text" ... value="Hola" class="helloInput">
...
</div>
<div class="flex-form-div">
...
    <input id="flexident4" name="flex1" type="text" ... value="Ciao" class="helloInput">
...
</div>

Renders a radio button field with the given title and option labels attached.
No option is selected when the form is loaded.

>>> let labels = ["this one", "or rather that one", "I just cannot decide"]
>>> printWidget "en" $ formify (Nothing @SingleChoiceSelection) [[buttons Vertical "Make your choice" labels]]
<div class="flex-form-div">
...
    <label for="flexident1">
      Make your choice
    </label>
...
        <label for="flexident1-1">
          <div>
            <input id="flexident1-1" type="radio" ... value="1" ...>
            this one
          </div>
        </label>
...
        <label for="flexident1-2">
          <div>
            <input id="flexident1-2" type="radio" ... value="2" ...>
            or rather that one
          </div>
        </label>
...
        <label for="flexident1-3">
          <div>
            <input id="flexident1-3" type="radio" ... value="3" ...>
            I just cannot decide
          </div>
        </label>
...
</div>
-}
formify
  :: (Formify a)
  => Maybe a -- ^ Optional default value for form.
  -> [[FieldInfo]] -- ^ Structure of form.
  -> Rendered Widget -- ^ Rendered form.
formify = checkAndApply joinRenders


{- |
like `formify`, but yields the individual sub-renders instead of a combined form.
Retains the layout structure given by the `FieldInfo` list argument.
This can be used in custom forms to incorporate generated inputs.
-}
formifyComponents :: Formify a => Maybe a -> [[FieldInfo]] -> Rendered [[Widget]]
formifyComponents = checkAndApply (fmap (tupleSequence . mapM sequence) . mapM sequence)
  where tupleSequence = fmap (joinAndPart . map joinAndPart)


{- |
like `formifyComponents`, but takes a simple list of `FieldInfo` values.
The sub-renders will also be returned as a flat list without any additional structure.
-}
formifyComponentsFlat :: Formify a => Maybe a -> [FieldInfo] -> Rendered [Widget]
formifyComponentsFlat ma = checkAndApply (fmap (tupleSequence . sequence) . sequence . concat) ma . (:[])
  where tupleSequence = fmap joinAndPart


joinAndPart :: [([a],b)] -> ([a],[b])
joinAndPart = first concat . unzip


checkAndApply
  :: Formify a
  => ([[Rendered Widget]] -> b)
  -> Maybe a
  -> [[FieldInfo]]
  -> b
checkAndApply toOutput ma xs = case rest of
    ([]:ns)
      | null ns   -> toOutput renders
    _ -> error $
      "The form generation did not use up all supplied FieldSettings values. " ++
      "Confirm your field type make sense with the amount of given FieldInfo values."
  where
    (rest, renders) = formifyImplementation ma xs


renderNextField
  ::  (FieldInfo ->
        ( FieldSettings FlexForm
        , Bool
        , FieldSettings FlexForm -> Maybe a -> AForm Handler a
        )
      )
  -> Maybe a
  -> [[FieldInfo]]
  -> ([[FieldInfo]], [[Rendered Widget]])
renderNextField _ _ [] = error "Ran out of FieldInfo values before finishing the form!"
renderNextField h ma ((x : xs) : xss) =
  let
    (lab, newId, g) = h x
  in
    (xs:xss, [[renderForm newId (`g` ma) lab]])
renderNextField _ _ _ = error "Incorrect FieldInfo for a field or single/multi choice!"

{- |
Premade `formifyImplementation` for types with `BaseForm` instances.
Use within manual instances of `Formify`.

=== __Example__

>>> instance BaseForm MyCoolType where baseForm = convertField toCool fromCool basisField
>>> instance Formify MyCoolType where formifyImplementation = formifyInstanceBasicField
-}
formifyInstanceBasicField
    :: BaseForm a
    => Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
formifyInstanceBasicField = renderNextField
  (\case
      Single fs -> (fs, True, areq baseForm)
      InternalListElem fs -> (fs, False, areq baseForm)
      _ -> error "Incorrect FieldInfo for a basic field. Use 'single'!"
  )

{- |
Same as `formifyInstanceBasicField`, but for optional fields with `Maybe` wrapping.

=== __Example__

>>> instance BaseForm MyCoolType where baseForm = convertField toCool fromCool basisField
>>> instance Formify (Maybe MyCoolType) where formifyImplementation = formifyInstanceOptionalField
-}
formifyInstanceOptionalField
    :: BaseForm a
    => Maybe (Maybe a)
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
formifyInstanceOptionalField = renderNextField
  (\case
      Single fs -> (fs, True, aopt baseForm)
      InternalListElem fs -> (fs, False, aopt baseForm)
      _ -> error "Incorrect FieldInfo for an optional basic field. Use 'single'!"
  )


formifyInstanceList
    :: (Formify a)
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
formifyInstanceList _ [] = error "Ran out of FieldInfo values before finishing the form!"
formifyInstanceList _ ((List _ [] : _) : _) = error "List used without supplying any FieldInfo values!"
formifyInstanceList mas ((List align (f:fs) : xs) : xss) =
    ( xs:xss
    , case align of
        Horizontal -> [concat $ firstRender ++ followingRenders]
        Vertical   -> firstRender ++ followingRenders
    )
  where
    defaults = case mas of
      Nothing -> repeat Nothing
      Just ds
        | length ds /= length fs +1
          -> error $
              "The default value contains too many/few individual values. " ++
              "It does not match the amount of FieldInfo supplied."
        | otherwise
          -> sequence mas

    headError [] = error "Defaults should never be empty here!"
    headError (x:_) = x

    firstRender = snd $ formifyImplementation (headError defaults) [[single f]]
    renderRest def fSettings = formifyImplementation def [[InternalListElem fSettings]]
    followingRenders = concat [snd $ renderRest d fSet | (d,fSet) <- zip (drop 1 defaults) fs]

formifyInstanceList _ _ = error "Incorrect FieldInfo for a list of fields! Use one of the list builders."



{- |
Premade `formifyImplementation` for "single choice" forms of enum types.
Use within manual instances of `Formify`.

Intended for use with types such as

@
data MyType = One | Two | Three deriving (Bounded, Enum, Eq, Show)
@

that cannot use a bodyless `Formify` instance.

=== __Examples__

>>> instance Formify MyType where formifyImplementation = formifyInstanceSingleChoice

>>> printWidget "en" $ formify (Just Two) [[buttonsEnum Horizontal "Choose one" (showToUniversalLabel @MyType)]]
...
<div class="flex-form-div">
...
    <label for="flexident1">
      Choose one
    </label>
...
        <label for="flexident1-1">
          <input id="flexident1-1" type="radio" ... value="1" ...>
          One
        </label>
        <label for="flexident1-2">
          <input id="flexident1-2" type="radio" ... value="2" checked ...>
          Two
        </label>
        <label for="flexident1-3">
          <input id="flexident1-3" type="radio" ... value="3" ...>
          Three
        </label>
...
</div>

>>> printWidget "en" $ formify (Just Two) [[dropdownEnum "Choose one" (showToUniversalLabel @MyType)]]
<div class="flex-form-div">
...
    <label for="flexident1">
      Choose one
    </label>
    <select id="flexident1" ...>
      <option value="1">
        One
      </option>
      <option value="2" selected>
        Two
      </option>
      <option value="3">
        Three
      </option>
    </select>
...
</div>
-}
formifyInstanceSingleChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
formifyInstanceSingleChoice = renderNextSingleChoiceField zipWithEnum

renderNextSingleChoiceField
    :: Eq a
    => ([SomeMessage FlexForm] -> [(SomeMessage FlexForm, a)])
    -> Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
renderNextSingleChoiceField pairsWith =
  renderNextField
  (\case
      ChoicesDropdown fs opts ->
        ( fs
        , True
        , areq $ selectField $ withOptions opts
        )
      ChoicesButtons align fs opts ->
        ( fs
        , True
        , areq $ case align of
            Vertical -> radioField
            Horizontal -> horizontalRadioField
          $ withOptions opts
        )
      _ -> error "Incorrect FieldInfo for a single choice field! Use one of the 'buttons' or 'dropdown' functions."
  )
  where withOptions = optionsPairs . pairsWith

renderNextMultipleChoiceField
    :: Eq a
    => ([SomeMessage FlexForm] -> [(SomeMessage FlexForm, a)])
    -> Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
renderNextMultipleChoiceField pairsWith =
  renderNextField
  (\case
      ChoicesDropdown fs opts ->
        ( fs
        , True
        , areq $ multiSelectField $ withOptions opts
        )
      ChoicesButtons align fs opts ->
        ( fs
        , True
        , areq $ case align of
            Vertical   -> checkboxField True
            Horizontal -> checkboxField False
          $ withOptions opts
        )
      _ -> error "Incorrect FieldInfo for a multiple choice field! Use one of the 'buttons' or 'dropdown' functions."
  )
  where withOptions = optionsPairs . pairsWith



{- |
Same as `formifyInstanceSingleChoice`, but for multiple choice.
This means the rendered input form will accept any number of inputs, resulting in a list of values.
Possible builders to use with instances are `buttonsEnum` (checkboxes) and `dropdownEnum` (select list).

=== __Examples__

>>> instance Formify [MyType] where formifyImplementation = formifyInstanceMultiChoice

>>> printWidget "en" $ formify (Just [Two,Three]) [[buttonsEnum Horizontal "Choose" (showToUniversalLabel @MyType)]]
<div class="flex-form-div">
...
    <label for="flexident1">
      Choose
    </label>
...
...
      <label>
        <input type="checkbox" ... value="1">
        One
      </label>
      <label>
        <input type="checkbox" ... value="2" checked>
        Two
      </label>
      <label>
        <input type="checkbox" ... value="3" checked>
        Three
      </label>
...
</div>

>>> printWidget "en" $ formify (Just [Two,Three]) [[dropdownEnum "Choose some" (showToUniversalLabel @MyType)]]
<div class="flex-form-div">
...
    <label for="flexident1">
      Choose some
    </label>
    <select id="flexident1" ... multiple>
      <option value="1">
        One
      </option>
      <option value="2" selected>
        Two
      </option>
      <option value="3" selected>
        Three
      </option>
    </select>
...
</div>
-}
formifyInstanceMultiChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
formifyInstanceMultiChoice = renderNextMultipleChoiceField zipWithEnum



zipWithEnum :: forall a. (Bounded a, Enum a) => [SomeMessage FlexForm] -> [(SomeMessage FlexForm, a)]
zipWithEnum labels
  | equalLength labels options = zip labels options
  | otherwise = error "Labels list and options list are of different lengths in an Enum choice form."
  where options = [minBound .. maxBound :: a]






{- |
Same as `buttons`, but using an explicit enum type.
Use this with custom enum types to automatically create labels
for all constructors according to the given showing scheme.

See `formifyInstanceSingleChoice`, `formifyInstanceMultiChoice` for example use.
-}
buttonsEnum
  :: (Bounded a, Enum a)
  => Alignment
  -> FieldSettings FlexForm      -- ^ FieldSettings for option input
  -> (a -> SomeMessage FlexForm) -- ^ Function from enum type values to labels.
  -> FieldInfo
buttonsEnum align t f = ChoicesButtons align t $ map f [minBound .. maxBound]



{- |
Create FieldInfo for a button field.
Will turn into either radio buttons or checkboxes
depending on the form type.
Use with `SingleChoiceSelection` or `MultipleChoiceSelection`.
__Do not use with custom enum types.__
__Use `buttonsEnum` instead.__

See `SingleChoiceSelection`, `MultipleChoiceSelection` for example use.
-}
buttons
  :: Alignment
  -> FieldSettings FlexForm -- ^ FieldSettings for option input
  -> [SomeMessage FlexForm] -- ^ Option labels
  -> FieldInfo
buttons = ChoicesButtons



{- |
Same as `dropdown`, but using an explicit enum type.
Use this with custom enum types to automatically create labels
for all constructors according to the given showing scheme.

See `formifyInstanceSingleChoice`, `formifyInstanceMultiChoice` for example use.
-}
dropdownEnum
  :: (Bounded a, Enum a)
  => FieldSettings FlexForm      -- ^ FieldSettings for select input
  -> (a -> SomeMessage FlexForm) -- ^ Function from enum type values to labels.
  -> FieldInfo
dropdownEnum t f = ChoicesDropdown t $ map f [minBound .. maxBound]



{- |
Create FieldInfo for a dropdown menu field.
Will turn into either single or multiple selection field
depending on the form type.
Use with `SingleChoiceSelection` or `MultipleChoiceSelection`.
__Do not use with custom enum types.__
__Use `dropdownEnum` instead.__

See `SingleChoiceSelection`, `MultipleChoiceSelection` for example use.
-}
dropdown
  :: FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [SomeMessage FlexForm]  -- ^ Option labels
  -> FieldInfo
dropdown = ChoicesDropdown



{- |
Create FieldInfo for a number of fields.
Their result will be handled as a list of values.
The length of the list is equal to the amount of labels provided.

=== __Example__

>>> let labels = ["Input 1", "Input 2", "Input 3"]
>>> printWidget "en" $ formify (Nothing @[Double]) [[list Horizontal labels]]
<div class="flex-form-div">
...
    <label for="flexident1">
      Input 1
    </label>
    <input id="flexident1" ... type="number" step="any" ...>
...
    <label for="flexident2">
      Input 2
    </label>
    <input id="flexident2" ... type="number" step="any" ...>
...
    <label for="flexident3">
      Input 3
    </label>
    <input id="flexident3" ... type="number" step="any" ...>
...
</div>
-}
list
  :: Alignment
  -> [FieldSettings FlexForm] -- ^ FieldSettings of individual fields
  -> FieldInfo
list = List



{- |
Same as `list`, but without using any field labels.
Attributes and CSS classes for each field cannot be set with this function.
Instead, all fields share the given list of attributes.
Use `list` if individual configuration is required.

See `formify` for example use.
-}
listWithoutLabels
  :: Alignment
  -> Int           -- ^ Amount of fields
  -> [(Text,Text)] -- ^ List of attribute and value pairs (attribute "class" for classes)
  -> FieldInfo
listWithoutLabels align amount attrs = List align $ replicate amount $ "" {fsAttrs = attrs}



{- |
Create FieldInfo for a standalone field.

See `formify` for example use.
-}
single :: FieldSettings FlexForm -> FieldInfo
single = Single
