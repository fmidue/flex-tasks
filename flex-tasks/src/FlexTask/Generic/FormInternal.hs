{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DeriveGeneric #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module FlexTask.Generic.FormInternal (
  module FlexTask.Generic.FormInternal
  ) where


import Control.Monad                    (join)
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
import Data.Text            (Text, pack)
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
>>> import Data.Text (Text)
>>> data MyType = One | Two | Three deriving (Bounded, Enum, Eq, Show)
>>> data MyCoolType = Yes | No deriving (Generic, Eq)
>>> instance Formify MyCoolType
>>> let toCool b = if b then Yes else No
>>> let fromCool c = c == Yes
>>> let existingField = baseField
-}



newtype MultipleChoice a = MultipleChoice
  { getAs :: [a]
  } deriving (Eq, Show)


data TypeField a where
  Basic :: BaseField a => (FieldSettings FlexForm) -> TypeField a
  SingleChoiceField :: Eq a => ChoiceShape -> (FieldSettings FlexForm) -> [(SomeMessage FlexForm, a)] -> TypeField a
  MultipleChoiceField :: Eq a => ChoiceShape -> (FieldSettings FlexForm) -> [(SomeMessage FlexForm, a)] -> TypeField (MultipleChoice a)


{- |
The layouting data type.
Each value is a form fragment parametrized by the overall type of the complete form
and the type of the fragment itself.
The overall type is given as a plain normal type,
while the type of the fragment is a type level list of types.
This is to allow for fragments with multiple types.

For example, for a product type

@
data Person = Person {name :: String, age :: Int, occupation :: String}
@

we might want to define a form fragment that contains only the first two parts of the record,
but is locked into becoming a @Person@ form:

@
nameAgePiece :: FormPiece Person '[String,Int]
@

or we could leave the overall type variable, so any type consisting of `String` and `Int` in that order,
e.g. @(String,Int)@ could use the fragment:

@
stringIntPiece :: FormPiece t '[String,Int]
@

You will mostly be able to use the simpler type synonyms `SimpleFormPiece`, `AnyFormPiece`
or `CompleteForm` to avoid dealing with type level lists.
-}
data FormPiece finalType fields where
  Single :: TypeField a -> SimpleFormPiece t a
  Combine :: Alignment -> FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
  List :: Alignment -> [TypeField a] -> ListFormPiece t a


{- |
Alias for a `FormPiece` whose overall type is the same as the fragment's.
This means the form is finished and no further pieces can be added.
-}
type CompleteForm a = AnyFormPiece a a

{- |
Alias for a `FormPiece` with exactly one type
that avoids having to write out the type level list.
-}
type SimpleFormPiece t a = FormPiece t (OneField a)

type ListFormPiece t a = FormPiece t (ManyFields a)

{- |
Alias for a `FormPiece` with arbitrarily many types
that avoids having to write out the type level list.
-}
type AnyFormPiece t a = FormPiece t (FormTypes a)


-- | Inner alignment of input field elements.
data Alignment = Horizontal | Vertical deriving (Eq,Show)


data ChoiceShape = Buttons Alignment | Dropdown


{- |
Wrapper type for generating hidden fields.
This can be used to transfer static information through the form to parsing.
Note that the generated field still has a label.
If the label is not left blank, then it will be displayed as normal.

=== __Example__

>>> printWidget "en" $ formify (Just $ Hidden 3) $ single $ basic ""
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

>>> printWidget "en" $ formify @(SingleInputList String) Nothing $ single $ basic "Input comma separated sentences"
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
>>> printWidget "en" $ formify (Just $ singleChoiceAnswer 3) $ single $ singleChoice Dropdown "Choose one" labels
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
>>> printWidget "en" $ formify (Just $ multipleChoiceAnswer [1,2]) $ single $ multipleChoice Dropdown "Choose one" labels
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
type MultipleChoiceSelection = MultipleChoice SingleChoiceSelection

{- |
Retrieve the list of selected options.
The first selectable option is @1@.
@[]@ if none are selected.
-}
getAnswers :: MultipleChoiceSelection -> [Int]
getAnswers = map getAnswer . getAs

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
multipleChoiceEmpty = MultipleChoice []

{- |
Value with given list of options selected.
The order of list elements is inconsequential.
-}
multipleChoiceAnswer :: [Int] -> MultipleChoiceSelection
multipleChoiceAnswer = MultipleChoice . map singleChoiceAnswer . nubSort



{- |
Types that can be represented as a simple Yesod input field.
A `BaseField` instance of type @a@ is needed if @a@ requires a type specific input method,
i.e. it is not just a wrapping newtype or product type.

Basic types are already instances of this class,
so you should not need to write your own instances in most cases.

Nethertheless, an instance can be given manually using the `Field` constructor
or the `convertField` function on an existing `Field`.

=== __Example__

>>> instance BaseField MyCoolType where baseField = convertField toCool fromCool existingField
-}
class (Formify a, FormTypes a ~ OneField a) => BaseField a where
  baseField :: Field Handler a


instance BaseField Integer where
  baseField = intField

instance BaseField Int where
  baseField = intField

instance BaseField Text where
  baseField = textField


instance BaseField Textarea where
  baseField = textareaField


instance BaseField Bool where
  baseField = boolField


instance BaseField Double where
  baseField = doubleField


instance PathPiece a => PathPiece (Hidden a) where
  fromPathPiece = fmap Hidden . fromPathPiece
  toPathPiece = toPathPiece . getHidden


instance (Formify a, PathPiece a) => BaseField (Hidden a) where
  baseField = hiddenField


-- This indicates I should probably change this class to something more succinct.
-- The first function is never used, since it normally handles the parsing.
instance Show a => BaseField (SingleInputList a) where
  baseField = convertField undefined (pack . intercalate ", " . map show . getList) textField


{- |
Class for generic generation of Html input forms for a given type.
Bodyless instances can be declared for any type instancing Generic.
__Exception: Types with multiple constructors.__
Use utility functions for those or provide your own instance.
-}
class Formify a where

  type FormTypes (a :: Type) :: Type
  type FormTypes a = GFormTypes a (Rep a)

  formDefaults :: Maybe a -> TypeList (FormTypes a)

  default formDefaults
    :: ( Generic a
       , GFormDefaults a (Rep a)
       , FormTypes a ~ GFormTypes a (Rep a)
       )
    => Maybe a
    -> TypeList (FormTypes a)
  formDefaults = gFormDefaults @a . fmap from


combineWidgets
  :: Alignment
  -> Rendered [[a]]
  -> Rendered [[a]]
  -> Rendered [[a]]
combineWidgets align f1 f2 = do
    res1 <- f1
    res2 <- f2
    pure $ do
      (ids1,names1,xss) <- res1
      (ids2,names2,yss) <- res2
      pure
        ( ids1 ++ ids2
        , nubOrd $ names1 ++ names2
        , appendWidgets xss yss)
  where
    appendWidgets = case align of
      Vertical -> (++)
      Horizontal -> zipWithLongest (\xs ys -> concat $ catMaybes [xs,ys])


instance Formify Integer where
  type FormTypes Integer = OneField Integer
  formDefaults = singleFormDefaults


instance Formify Int where
  type FormTypes Int = OneField Int
  formDefaults = singleFormDefaults

instance Formify Text where
  type FormTypes Text = OneField Text
  formDefaults = singleFormDefaults


instance Formify Textarea where
  type FormTypes Textarea = OneField Textarea
  formDefaults = singleFormDefaults


instance Formify Bool where
  type FormTypes Bool = OneField Bool
  formDefaults = singleFormDefaults


instance Formify Double where
  type FormTypes Double = OneField Double
  formDefaults = singleFormDefaults


instance Formify (Hidden a) where
  type FormTypes (Hidden a) = OneField (Hidden a)
  formDefaults = singleFormDefaults


instance Formify (SingleInputList a) where
  type FormTypes (SingleInputList a) = OneField (SingleInputList a)
  formDefaults = singleFormDefaults


instance (Formify a, Formify b) => Formify (a,b)

instance (Formify a, Formify b, Formify c) => Formify (a,b,c)

instance (Formify a, Formify b, Formify c, Formify d) => Formify (a,b,c,d)

instance (Formify a, Formify b, Formify c, Formify d, Formify e) => Formify (a,b,c,d,e)

instance (Formify a, Formify b, Formify c, Formify d, Formify e, Formify f) => Formify (a,b,c,d,e,f)


instance {-# Overlappable #-} Formify a => Formify [a] where
  type FormTypes [a] = ManyFields (SingleInputType (FormTypes a))
  formDefaults mValues = TMany t a
    where
      t = getSingleDefault $ formDefaults @a Nothing
      a = map (getSingleDefault . formDefaults . Just) <$> mValues


instance Formify (Maybe a) where
  type FormTypes (Maybe a) = OneField a
  formDefaults m = TOne $ OptionalDefault $ join m


instance Formify SingleChoiceSelection where
  type FormTypes SingleChoiceSelection = OneField SingleChoiceSelection
  formDefaults = singleFormDefaults


instance Formify (MultipleChoice a) where
  type FormTypes (MultipleChoice a) = OneField (MultipleChoice a)
  formDefaults = singleFormDefaults


{- |
This is the main way to build generic forms.
Use in conjunction with `FieldInfo` builders to generate a form.

Will fail if remaining `FieldInfo` structure is not empty,
indicating the form is faulty.


=== __Examples__

Renders an input field with /type=number/ attribute, no default value and label /Age/.

>>> printWidget "en" $ formify @Int Nothing $ single $ basic "Age"
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

>>> let defaults = ["Hallo", "Hello", "Hola", "Ciao" :: Text]
>>> printWidget "en" $ formify (Just defaults) $ listWithoutLabels Vertical 4 basic [("class","helloInput")]
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
>>> printWidget "en" $ formify @SingleChoiceSelection Nothing $ single $ singleChoice (Buttons Vertical) "Make your choice" labels
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
formify mDefault = applyToWidget joinWidgets . formifyComponents mDefault


{- |
like `formify`, but yields the individual sub-renders instead of a combined form.
Retains the layout structure given by the `FieldInfo` list argument.
This can be used in custom forms to incorporate generated inputs.
-}
formifyComponents
  :: Formify a
  => Maybe a
  -- ^ Optional default value for form
  -> CompleteForm a
  -- ^ Structure and type of form
  -> Rendered [[Widget]]
  -- ^ sub-renders
formifyComponents mDefault = renderLayout (formDefaults mDefault)


{- |
like `formifyComponents`, but takes a simple list of `FieldInfo` values.
The sub-renders will also be returned as a flat list without any additional structure.
-}
formifyComponentsFlat :: Formify a => Maybe a -> CompleteForm a -> Rendered [Widget]
formifyComponentsFlat mDefault = applyToWidget concat . formifyComponents mDefault


renderRequiredness :: OneDefault a -> TypeField a -> Rendered Widget
renderRequiredness (OptionalDefault mDefault) field = renderField (\f fs -> aopt f fs $ Just mDefault) field
renderRequiredness (RequiredDefault mDefault) field = renderField (\f fs -> areq f fs mDefault) field


renderField :: (Field Handler a  -> FieldSettings FlexForm -> AForm Handler c) -> TypeField a -> Rendered Widget
renderField req info = case info of
  Basic fs -> renderForm (req baseField) fs
  SingleChoiceField k fs xs -> renderForm (req $ case k of
    Dropdown -> selectField $ optionsPairs xs
    Buttons Vertical -> radioField True $ optionsPairs xs
    Buttons Horizontal -> radioField False $ optionsPairs xs) fs
  MultipleChoiceField k fs xs -> renderForm (req $
    convertField MultipleChoice getAs $ case k of
      Dropdown -> multiSelectField $ optionsPairs xs
      Buttons Vertical -> checkboxField True $ optionsPairs xs
      Buttons Horizontal -> checkboxField False $ optionsPairs xs) fs


renderLayout :: TypeList a -> FormPiece t a -> Rendered [[Widget]]
renderLayout (TOne mDefault) (Single x) = applyToWidget (singleton . singleton) $
  renderRequiredness mDefault x
renderLayout mDefault (Combine align x y) = renderLayout a x `how` renderLayout b y
  where
    (a,b) = splitTypeList (pieceShape x) mDefault
    how = combineWidgets align
renderLayout (TMany t mDefault) (List align fs) =
    foldr1 addParams [renderLayout (TOne d) (Single f) | (d,f) <- zip defaults fs]
  where
    defaults = case mDefault of
      Nothing -> repeat t
      Just ds
        | length ds /= length fs
          -> error
              "Lengths of form default value and FieldInfo list do not match!"
        | otherwise -> ds


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


basic :: BaseField a => FieldSettings FlexForm -> TypeField a
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
singleChoiceEnum shape fs = SingleChoiceField shape fs . optionsFromType



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
singleChoice shape fs = SingleChoiceField shape fs . options


multipleChoice
  :: ChoiceShape
  -> FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [SomeMessage FlexForm]  -- ^ Option labels
  -> TypeField MultipleChoiceSelection
multipleChoice shape fs = MultipleChoiceField shape fs . options


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
  -> TypeField (MultipleChoice a)
multipleChoiceEnum shape fs = MultipleChoiceField shape fs . optionsFromType


{- |
Create FieldInfo for a standalone field.
See `formify` for example use.
-}
single :: TypeField a -> SimpleFormPiece t a
single = Single


{- |
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
infixr 5 >|
(>|) :: FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
(>|) = Combine Horizontal


beside :: FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
beside = (>|)


infixr 4 >-
(>-) :: FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
(>-) = Combine Vertical


above :: FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
above = (>-)


{- |
Create FieldInfo for a number of basic fields.
Their result will be handled as a list of values.
Use for lists of BaseField fields like `Int`, `String`, `Double`.
The length of the list is equal to the amount of labels provided.

=== __Example__

>>> let labels = ["Input 1", "Input 2", "Input 3"]
>>> printWidget "en" $ formify @[Double] Nothing $ list Horizontal basic labels
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
{-
Create FieldInfo for a number of arbitrary fields.
Takes the builder to repeatedly use for each field
and a list of values to use it on.
Their result will be handled as a list of values.
Use to render lists of dropdown or button fields with different labels.
-}
list
  :: Alignment
  -> (a -> TypeField b)
  -> [a]
  -> ListFormPiece t b
list align builder = List align . map builder


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
  -> (FieldSettings FlexForm -> TypeField a)
  -> [(Text,Text)] -- ^ List of attribute and value pairs (attribute "class" for classes)
  -> ListFormPiece t a
listWithoutLabels align amount req attrs =
  list align req $ replicate amount "" {fsAttrs = attrs}


{- |
Create FieldInfo for a list containing exact copies the specified field.
The results of the copies will be handled as a list of values.
Use to render lists of dropdown or button fields with identical labels.
-}
listRepeatedly
  :: Alignment
  -> Int       -- ^ How many copies
  -> TypeField a -- ^ The field to multiply
  -> ListFormPiece t a
listRepeatedly alignment amount = list alignment id . replicate amount


options :: [a] -> [(a, SingleChoiceSelection)]
options opts = zip opts $ map SingleChoiceSelection [1..]


optionsFromType :: (Bounded b, Enum b) => (b -> a) -> [(a, b)]
optionsFromType f = map (\x -> (f x, x)) [minBound .. maxBound]


-- Type Machinery --


data TypeList xs where
  TOne :: OneDefault x -> TypeList (OneField x)
  TMany :: OneDefault x -> Maybe [OneDefault x] -> TypeList (ManyFields x)
  TCons :: InputDefault x -> TypeList xs -> TypeList (x :> xs)


infixr 5 ++
type family xs ++ ys :: Type where
  OneField a ++ ys = OneField a :> ys
  ManyFields a ++ ys = ManyFields a :> ys
  (x :> xs) ++ ys = x :> (xs ++ ys)


singleFormDefaults :: Maybe a -> TypeList (OneField a)
singleFormDefaults x = TOne (RequiredDefault x)


appendTypeList :: TypeList xs -> TypeList ys -> TypeList (xs ++ ys)
appendTypeList (TOne x) = TCons $ OneInputDefault x
appendTypeList (TMany t xs) = TCons $ ManyInputDefaults t xs
appendTypeList (TCons x xs) = TCons x . appendTypeList xs


data TypeShape xs where
  OneShape  :: TypeShape (OneField a)
  ManyShape :: TypeShape (ManyFields a)
  ConsShape :: TypeShape xs -> TypeShape (x :> xs)


splitTypeList :: TypeShape xs -> TypeList (xs ++ ys) -> (TypeList xs, TypeList ys)
splitTypeList OneShape (TCons (OneInputDefault y) ys) = (TOne y, ys)
splitTypeList ManyShape (TCons (ManyInputDefaults t y) ys) = (TMany t y, ys)
splitTypeList (ConsShape xs) (TCons y ys) = first (TCons y) $ splitTypeList xs ys


pieceShape :: FormPiece t xs -> TypeShape xs
pieceShape Single {} = OneShape
pieceShape List {} = ManyShape
pieceShape (Combine _ x y) = appendShape (pieceShape x) $ pieceShape y


appendShape :: TypeShape xs -> TypeShape ys -> TypeShape (xs ++ ys)
appendShape OneShape ys = ConsShape ys
appendShape ManyShape ys = ConsShape ys
appendShape (ConsShape xs) ys = ConsShape (appendShape xs ys)


type family GFormTypes original rep :: Type where
  GFormTypes original (M1 i metadata fields) = GFormTypes original fields

  GFormTypes original (K1 i field) = FormTypes field

  GFormTypes original (left :*: right) =
    GFormTypes original left ++
    GFormTypes original right

  GFormTypes original (left :+: right) = OneField original


class GFormDefaults original rep where
  gFormDefaults :: Maybe (rep p) -> TypeList (GFormTypes original rep)


instance GFormDefaults original fields => GFormDefaults original (M1 i metadata fields) where
  gFormDefaults = gFormDefaults @original . fmap unM1


instance Formify field => GFormDefaults original (K1 i field) where
  gFormDefaults = formDefaults . fmap unK1


instance
  ( GFormDefaults original left
  , GFormDefaults original right
  )
  => GFormDefaults original (left :*: right) where

  gFormDefaults Nothing = appendTypeList
    (gFormDefaults @original @left Nothing)
    (gFormDefaults @original @right Nothing)
  gFormDefaults (Just (left :*: right)) = appendTypeList
    (gFormDefaults @original $ Just left)
    (gFormDefaults @original $ Just right)


instance {-# Overlapping #-}
  ( Generic original
  , Rep original ~ D1 metadata (left :+: right)
  , NullarySum (left :+: right)
  )
  => GFormDefaults original (D1 metadata (left :+: right))
  where
  gFormDefaults m = singleFormDefaults (fmap to m)


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


data OneField a
data ManyFields a


infixr 6 :>
data x :> xs


type family SingleInputType fields :: Type where
  SingleInputType (OneField a) = a

  SingleInputType fields = TypeError
    ( 'Text "This type does not correspond to exactly one input field."
    )


getSingleDefault :: TypeList fields -> OneDefault (SingleInputType fields)
getSingleDefault (TOne d) = d
getSingleDefault _ = error "unreachable: SingleInputType rejected this form shape"


data OneDefault a
  = RequiredDefault (Maybe a)
  | OptionalDefault (Maybe a)


data InputDefault a where
  OneInputDefault :: OneDefault a -> InputDefault (OneField a)
  ManyInputDefaults :: OneDefault a -> Maybe [OneDefault a] -> InputDefault (ManyFields a)
