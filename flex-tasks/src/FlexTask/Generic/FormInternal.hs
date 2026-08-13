{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DeriveGeneric #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module FlexTask.Generic.FormInternal (
  module FlexTask.Generic.FormInternal
  ) where


import Data.Kind                        (Constraint, Type)
import Data.List.Extra (
  intercalate,
  nubOrd,
  nubSort,
  singleton,
  zipWithLongest,
  )
import Data.Maybe           (catMaybes)
import Data.Tuple.Extra     (first)
import GHC.Generics (
  Generic(..),
  (:+:),
  (:*:)(..),
  C,
  D1,
  K1(unK1),
  M1(unM1),
  U1,
  )
import GHC.TypeLits                     (ErrorMessage(..), TypeError)
import Data.Text            (Text, pack, unpack)
import Yesod (
  AForm,
  Field,
  FieldSettings(..),
  PathPiece(..),
  SomeMessage,
  Textarea,
  areq,
  aopt,
  boolField,
  convertField,
  doubleField,
  hiddenField,
  intField,
  multiSelectField,
  optionsPairs,
  textareaField,
  textField,
  )

import FlexTask.FormUtil    (applyToWidget)
import FlexTask.Widgets
  ( checkboxField
  , radioField
  , joinWidgets
  , renderForm
  , selectField
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

=== __Examples__

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

__Caution: Not all horizontal alignments will display correctly.__
__For example, if two vertical lists are composed horizontally,__
__then the second list may not be longer than the first.__

Input

@
[[listWithoutLabels Vertical 2 [], listWithoutLabels Vertical 3 []]]
@

will __not__ result in

@
list11      list21

list12      list22

            list23
@

but instead in

@
list11     list21

list12     list22

list23
@
-}
data TypeField a where
  Basic :: BaseForm a => (FieldSettings FlexForm) -> TypeField a
  SingleChoice :: Eq a => ChoiceShape -> (FieldSettings FlexForm) -> [(SomeMessage FlexForm, a)] -> TypeField a
  MultipleChoice :: Eq a => ChoiceShape -> (FieldSettings FlexForm) -> [(SomeMessage FlexForm, a)] -> TypeField [a]


data Requiredness a where
  Required :: TypeField a -> Requiredness a
  Optional :: TypeField a -> Requiredness (Maybe a)


data FormLayout finalType fields where
  Single :: Requiredness a -> FormLayout t '[a]
  Beside :: SplitOff xs ys => FormLayout t xs -> FormLayout t ys -> FormLayout t (xs ++ ys)
  Above :: SplitOff xs ys => FormLayout t xs -> FormLayout t ys -> FormLayout t (xs ++ ys)
  List :: Alignment -> [Requiredness a] -> FormLayout t '[[a]]


type CompleteForm a = FormLayout a (FormTypes a)
type SimpleFormPiece t a = FormLayout t '[a]
type AnyFormPiece t a = FormLayout t (FormTypes a)


-- | Inner alignment of input field elements.
data Alignment = Horizontal | Vertical deriving (Eq,Show)


data ChoiceShape = Buttons Alignment | Dropdown


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
type MultipleChoiceSelection = [SingleChoiceSelection]

{- |
Retrieve the list of selected options.
The first selectable option is @1@.
@[]@ if none are selected.
-}
getAnswers :: MultipleChoiceSelection -> [Int]
getAnswers = map getAnswer

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
multipleChoiceEmpty = []

{- |
Value with given list of options selected.
The order of list elements is inconsequential.
-}
multipleChoiceAnswer :: [Int] -> MultipleChoiceSelection
multipleChoiceAnswer = map singleChoiceAnswer . nubSort



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

  type FormTypes a :: [Type]
  type FormTypes a = GFormTypes a (Rep a)

  formDefaults :: a -> TypeList (FormTypes a)

  default formDefaults
    :: ( Generic a
       , GFormDefaults a (Rep a)
       , FormTypes a ~ GFormTypes a (Rep a)
       )
    => a
    -> TypeList (FormTypes a)
  formDefaults = gFormDefaults @a . from

  formifyImplementation
      :: Maybe a -- ^ Optional default value for form.
      -> CompleteForm a -- ^ Structure and type of form.
      -> Rendered [[Widget]] -- ^ remaining form structure and completed sub-renders.
  formifyImplementation mDefault = renderLayout (formDefaults <$> mDefault)


horizontally
  :: Rendered [[a]]
  -> Rendered [[a]]
  -> Rendered [[a]]
f1 `horizontally` f2 = do
    res1 <- f1
    res2 <- f2
    pure $ do
      (ids1,names1,xss) <- res1
      (ids2,names2,yss) <- res2
      pure
        ( ids1 ++ ids2
        , nubOrd $ names1 ++ names2
        , zipWithLongest (\xs ys -> concat $ catMaybes [xs,ys]) xss yss)



vertically
  :: Rendered [[a]]
  -> Rendered [[a]]
  -> Rendered [[a]]
f1 `vertically` f2 = do
    res1 <- f1
    res2 <- f2
    pure $ do
      (ids1,names1,xss) <- res1
      (ids2,names2,yss) <- res2
      pure (ids1 ++ ids2, nubOrd $ names1 ++ names2, xss ++ yss)


instance Formify Integer where
  type FormTypes Integer = '[Integer]
  formDefaults = singleFormDefaults


instance Formify Int where
  type FormTypes Int = '[Int]
  formDefaults = singleFormDefaults

instance Formify Text where
  type FormTypes Text = '[Text]
  formDefaults = singleFormDefaults

instance Formify String where
  type FormTypes String = '[String]
  formDefaults = singleFormDefaults


instance Formify Textarea where
  type FormTypes Textarea = '[Textarea]
  formDefaults = singleFormDefaults


instance Formify Bool where
  type FormTypes Bool = '[Bool]
  formDefaults = singleFormDefaults


instance Formify Double where
  type FormTypes Double = '[Double]
  formDefaults = singleFormDefaults


instance Formify a => Formify (Hidden a) where
  type FormTypes (Hidden a) = FormTypes a
  formDefaults (Hidden a) = formDefaults a


instance Formify (SingleInputList a) where
  type FormTypes (SingleInputList a) = FormTypes [a]
  formDefaults (SingleInputList a) = formDefaults a


instance (Formify a, Formify b) => Formify (a,b)

instance (Formify a, Formify b, Formify c) => Formify (a,b,c)

instance (Formify a, Formify b, Formify c, Formify d) => Formify (a,b,c,d)

instance (Formify a, Formify b, Formify c, Formify d, Formify e) => Formify (a,b,c,d,e)

instance (Formify a, Formify b, Formify c, Formify d, Formify e, Formify f) => Formify (a,b,c,d,e,f)


instance {-# Overlappable #-} Formify [a] where
  type FormTypes [a] = '[[a]]
  formDefaults = singleFormDefaults


instance Formify (Maybe a) where
  type FormTypes (Maybe a) = '[Maybe a]
  formDefaults = singleFormDefaults


instance Formify SingleChoiceSelection where
  type FormTypes SingleChoiceSelection = '[SingleChoiceSelection]
  formDefaults = singleFormDefaults


{- |
This is the main way to build generic forms.
Use in conjunction with `FieldInfo` builders to generate a form.

Will fail if remaining `FieldInfo` structure is not empty,
indicating the form is faulty.


=== __Examples__

Renders an input field with /type=number/ attribute, no default value and label /Age/.

>>> printWidget "en" $ formify (Nothing @Int) [[single "Age"]]
<div class="flex-form-div form-group">
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
<div class="flex-form-div form-group">
...
    <input id="flexident1" ... type="text" ... value="Hallo" class="helloInput">
...
</div>
<div class="flex-form-div form-group">
...
    <input id="flexident2" ... type="text" ... value="Hello" class="helloInput">
...
</div>
<div class="flex-form-div form-group">
...
    <input id="flexident3" ... type="text" ... value="Hola" class="helloInput">
...
</div>
<div class="flex-form-div form-group">
...
    <input id="flexident4" ... type="text" ... value="Ciao" class="helloInput">
...
</div>

Renders a radio button field with the given title and option labels attached.
No option is selected when the form is loaded.

>>> let labels = ["this one", "or rather that one", "I just cannot decide"]
>>> printWidget "en" $ formify (Nothing @SingleChoiceSelection) [[buttons Vertical "Make your choice" labels]]
...
<div class="flex-form-div form-group">
...
    <label for="flexident1">
      Make your choice
    </label>
    <div>
      <span id="flexident1">
        <div>
          <label>
            <input id="flexident1-1" type="radio" ... value="1" ...>
            this one
          </label>
        </div>
        <div>
          <label>
            <input id="flexident1-2" type="radio" ... value="2" ...>
            or rather that one
          </label>
        </div>
        <div>
          <label>
            <input id="flexident1-3" type="radio" ... value="3" ...>
            I just cannot decide
          </label>
        </div>
      </span>
    </div>
...
</div>
-}
formify
  :: Formify a
  => Maybe a
  -- ^ Optional default value.
  -> CompleteForm a
  -- ^ Structure of the form.
  -> Rendered Widget
  -- ^ Rendered form.
formify mDefault = applyToWidget joinWidgets . formifyImplementation mDefault


{- |
like `formify`, but yields the individual sub-renders instead of a combined form.
Retains the layout structure given by the `FieldInfo` list argument.
This can be used in custom forms to incorporate generated inputs.
-}
formifyComponents :: Formify a => Maybe a -> CompleteForm a -> Rendered [[Widget]]
formifyComponents = formifyImplementation


{- |
like `formifyComponents`, but takes a simple list of `FieldInfo` values.
The sub-renders will also be returned as a flat list without any additional structure.
-}
formifyComponentsFlat :: Formify a => Maybe a -> CompleteForm a -> Rendered [Widget]
formifyComponentsFlat mDefault = applyToWidget concat . formifyImplementation mDefault


renderRequiredness :: Maybe a -> Requiredness a -> Rendered Widget
renderRequiredness mDefault (Optional field) = renderField (\f fs -> aopt f fs mDefault) field
renderRequiredness mDefault (Required field) = renderField (\f fs -> areq f fs mDefault) field


renderField :: (Field Handler a  -> FieldSettings FlexForm -> AForm Handler c) -> TypeField a -> Rendered Widget
renderField req info = case info of
  Basic fs -> renderForm (req baseForm) fs
  SingleChoice k fs xs -> renderForm (req $ case k of
    Dropdown -> selectField $ optionsPairs xs
    Buttons Vertical -> radioField True $ optionsPairs xs
    Buttons Horizontal -> radioField False $ optionsPairs xs) fs
  MultipleChoice k fs xs -> renderForm (req $ case k of
    Dropdown -> multiSelectField $ optionsPairs xs
    Buttons Vertical -> checkboxField True $ optionsPairs xs
    Buttons Horizontal -> checkboxField False $ optionsPairs xs) fs


renderLayout :: Maybe (TypeList a) -> FormLayout t a -> Rendered [[Widget]]
renderLayout mDefault (Single x) = applyToWidget (singleton . singleton) $
  flip renderRequiredness x $ fmap (\(TCons t TEmpty) -> t) mDefault
renderLayout mDefault (Beside x y) = renderLayout a x `horizontally` renderLayout b y
  where (a,b) = splitMaybeDefaults mDefault
renderLayout mDefault (Above x y) = renderLayout a x `vertically` renderLayout b y
  where (a,b) = splitMaybeDefaults mDefault
renderLayout mDefault (List align fs) =
    foldr1 addParams [renderLayout d (Single f) | (d,f) <- zip defaults fs]
  where
    defaults = case mDefault of
      Nothing -> repeat Nothing
      Just (TCons ds TEmpty)
        | length ds /= length fs
          -> error
              "Lengths of form default value and FieldInfo list do not match!"
        | otherwise
          -> map (Just . (`TCons` TEmpty)) ds

    addParams f1 f2 = do
      res1 <- f1
      res2 <- f2
      pure $ do
        (ids1,names1,wid1) <- res1
        (ids2,names2,wid2) <- res2
        pure
          ( ids1 ++ ids2
          , [nubOrd $ concat $ names1 ++ names2]
          , case align of
              Vertical   -> wid1 ++ wid2
              Horizontal -> [concat $ wid1 ++ wid2]
          )


basic :: BaseForm a => FieldSettings FlexForm -> TypeField a
basic = Basic


{- |
Same as `buttons`, but using an explicit enum type.
Use this with custom enum types to automatically create labels
for all constructors according to the given showing scheme.

See `formifyInstanceSingleChoice`, `formifyInstanceMultiChoice` for example use.
-}
singleChoiceEnum
  :: (Eq a, Bounded a, Enum a)
  => ChoiceShape
  -> FieldSettings FlexForm      -- ^ FieldSettings for select input
  -> (a -> SomeMessage FlexForm) -- ^ Function from enum type values to labels.
  -> TypeField a
singleChoiceEnum shape fs = SingleChoice shape fs . optionsFromType



{- |
Create FieldInfo for a button field.
Will turn into either radio buttons or checkboxes
depending on the form type.
Use with `SingleChoiceSelection` or `MultipleChoiceSelection`.
__Do not use with custom enum types.__
__Use `buttonsEnum` instead.__

See `SingleChoiceSelection`, `MultipleChoiceSelection` for example use.
-}
singleChoice
  :: ChoiceShape
  -> FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [SomeMessage FlexForm]  -- ^ Option labels
  -> TypeField SingleChoiceSelection
singleChoice shape fs = SingleChoice shape fs . options


multipleChoice
  :: ChoiceShape
  -> FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [SomeMessage FlexForm]  -- ^ Option labels
  -> TypeField MultipleChoiceSelection
multipleChoice shape fs = MultipleChoice shape fs . options


{- |
Create FieldInfo for a dropdown menu field.
Will turn into either single or multiple selection field
depending on the form type.
Use with `SingleChoiceSelection` or `MultipleChoiceSelection`.
__Do not use with custom enum types.__
__Use `dropdownEnum` instead.__

See `SingleChoiceSelection`, `MultipleChoiceSelection` for example use.
-}
multipleChoiceEnum
  :: (Eq a, Bounded a, Enum a)
  => ChoiceShape
  -> FieldSettings FlexForm
  -- ^ FieldSettings for select input
  -> (a -> SomeMessage FlexForm)
  -- ^ Function from enum type values to labels.
  -> TypeField [a]
multipleChoiceEnum shape fs = MultipleChoice shape fs . optionsFromType


required :: TypeField a -> Requiredness a
required = Required

optional :: TypeField a -> Requiredness (Maybe a)
optional = Optional

{- |
Create FieldInfo for a standalone field.
See `formify` for example use.
-}
single :: Requiredness a -> SimpleFormPiece t a
single = Single


infixl 5 >|
(>|) :: SplitOff xs ys => FormLayout t xs -> FormLayout t ys -> FormLayout t (xs ++ ys)
(>|) = Beside


beside :: SplitOff xs ys => FormLayout t xs -> FormLayout t ys -> FormLayout t (xs ++ ys)
beside = (>|)


infixl 4 >-
(>-) :: SplitOff xs ys => FormLayout t xs -> FormLayout t ys -> FormLayout t (xs ++ ys)
(>-) = Above


above :: SplitOff xs ys => FormLayout t xs -> FormLayout t ys -> FormLayout t (xs ++ ys)
above = (>-)


{- |
Create FieldInfo for a number of basic fields.
Their result will be handled as a list of values.
Use for lists of BaseForm fields like `Int`, `String`, `Double`.
The length of the list is equal to the amount of labels provided.

=== __Example__

>>> let labels = ["Input 1", "Input 2", "Input 3"]
>>> printWidget "en" $ formify (Nothing @[Double]) [[list Horizontal labels]]
<div class="flex-form-div form-group">
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
  -> (FieldSettings FlexForm -> Requiredness a)
  -> [FieldSettings FlexForm] -- ^ FieldSettings of individual fields
  -> SimpleFormPiece t [a]
list = repeatBuilderOn


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
  -> (FieldSettings FlexForm -> Requiredness a)
  -> [(Text,Text)] -- ^ List of attribute and value pairs (attribute "class" for classes)
  -> SimpleFormPiece t [a]
listWithoutLabels align amount req attrs =
  list align req $ replicate amount "" {fsAttrs = attrs}


{- |
Create FieldInfo for a number of arbitrary fields.
Takes the builder to repeatedly use for each field
and a list of values to use it on.
Their result will be handled as a list of values.
Use to render lists of dropdown or button fields with different labels.
-}
repeatBuilderOn
  :: Alignment
  -> (a -> Requiredness b) -- ^ FieldInfo builder to use
  -> [a]        -- ^ List of values to use builder on
  -> SimpleFormPiece t [b]
repeatBuilderOn align builder = List align . map builder


{- |
Create FieldInfo for a list containing exact copies the specified field.
The results of the copies will be handled as a list of values.
Use to render lists of dropdown or button fields with identical labels.
-}
repeatFieldInfo
  :: Alignment
  -> Int       -- ^ How many copies
  -> Requiredness a -- ^ The field to multiply
  -> SimpleFormPiece t [a]
repeatFieldInfo alignment amount = repeatBuilderOn alignment id . replicate amount


options :: [a] -> [(a, SingleChoiceSelection)]
options opts = zip opts $ map SingleChoiceSelection [1..]


optionsFromType :: (Bounded b, Enum b) => (b -> a) -> [(a, b)]
optionsFromType f = map (\x -> (f x, x)) [minBound .. maxBound]


-- Type Machinery --


data TypeList xs where
  TEmpty :: TypeList '[]
  TCons :: x -> TypeList xs -> TypeList (x ': xs)


singleFormDefaults :: a -> TypeList '[a]
singleFormDefaults x = TCons x TEmpty


appendTypeList :: TypeList xs -> TypeList ys -> TypeList (xs ++ ys)
appendTypeList TEmpty = id
appendTypeList (TCons x xs) = TCons x . appendTypeList xs


type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)


class SplitOff xs ys where
  splitTypeList :: TypeList (xs ++ ys) -> (TypeList xs, TypeList ys)


instance SplitOff '[] ys where
  splitTypeList = (TEmpty,)

instance SplitOff xs ys => SplitOff (x ': xs) ys where
  splitTypeList (TCons x rest) = first (TCons x) $ splitTypeList rest


splitMaybeDefaults :: SplitOff xs ys => Maybe (TypeList (xs ++ ys)) -> (Maybe (TypeList xs), Maybe (TypeList ys))
splitMaybeDefaults Nothing = (Nothing, Nothing)
splitMaybeDefaults (Just xs) = (Just left, Just right)
  where (left, right) = splitTypeList xs


type family GFormTypes original rep :: [Type] where
  GFormTypes original (M1 i metadata fields) = GFormTypes original fields

  GFormTypes original (K1 i field) = FormTypes field

  GFormTypes original (left :*: right) =
    GFormTypes original left ++ GFormTypes original right

  GFormTypes original (left :+: right) = '[original]


class GFormDefaults original rep where
  gFormDefaults :: rep p -> TypeList (GFormTypes original rep)


instance GFormDefaults original fields => GFormDefaults original (M1 i metadata fields) where
  gFormDefaults = gFormDefaults @original . unM1


instance Formify field => GFormDefaults original (K1 i field) where
  gFormDefaults = formDefaults . unK1


instance
  ( GFormDefaults original left
  , GFormDefaults original right
  )
  => GFormDefaults original (left :*: right) where

  gFormDefaults (left :*: right) = appendTypeList
    (gFormDefaults @original left)
    (gFormDefaults @original right)


instance {-# Overlapping #-}
  ( Generic original
  , Rep original ~ D1 metadata (left :+: right)
  , NullarySum (left :+: right)
  )
  => GFormDefaults original (D1 metadata (left :+: right))
  where
  gFormDefaults = singleFormDefaults . to


instance
    TypeError
      ( 'Text "Cannot derive Formify for a single constructor without fields."
        ':$$:
        'Text "This is either a constant value (if required) or a Boolean (if optional)."
      ) => GFormDefaults original U1 where
  gFormDefaults = undefined


type family NullarySum rep :: Constraint where
  NullarySum (left :+: right) = (NullarySum left, NullarySum right)

  NullarySum (M1 C metadata U1) = ()

  NullarySum (M1 C metadata fields) =
    TypeError
      ( 'Text "Cannot derive Formify for this sum type." ':$$:
        'Text "A sum type must contain only nullary constructors," ':$$:
        'Text "but at least one constructor contains fields." ':$$:
        'Text "Consider a manual Formify instance for this type."
      )
