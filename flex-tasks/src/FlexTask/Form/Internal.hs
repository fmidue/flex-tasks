{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module FlexTask.Form.Internal (
  module FlexTask.Form.Internal
  ) where


import Data.List.Extra (
  intercalate,
  nubOrd,
  singleton,
  zipWithLongest,
  )
import Data.Maybe           (catMaybes)
import Data.Text            (Text, pack)
import Yesod (
  AForm,
  Field,
  FieldSettings(..),
  PathPiece,
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

import FlexTask.Form.Formify            (Formify(..))
import FlexTask.Form.Util               (applyToWidget)
import FlexTask.Form.Widgets
  ( checkboxField
  , radioField
  , joinWidgets
  , renderForm
  , selectField
  )
import FlexTask.Form.TypeLevel
import FlexTask.Form.Types (
  FlexForm(..),
  Handler,
  Rendered,
  Widget,
  )
import FlexTask.InputTypes (
  Hidden(..),
  MultipleChoice(..),
  MultipleChoiceSelection,
  SingleChoiceSelection(..),
  SingleInputList(..),
  )


{- $setup
>>> :set -XTypeApplications
>>> :set -XDeriveGeneric
>>> :set -XOverloadedStrings
>>> import FlexTask.Form.Util
>>> import FlexTask.InputTypes
>>> import FlexTask.Form.Formify
>>> import Data.Text (Text)
>>> data MyType = One | Two | Three deriving (Bounded, Enum, Eq, Generic, Show)
>>> data MyCoolType = Yes | No deriving (Generic, Eq)
>>> instance Formify MyType
>>> instance Formify MyCoolType
>>> let toCool b = if b then Yes else No
>>> let fromCool c = c == Yes
>>> let existingField = baseField
-}


-- | Represents a specific input field associated with given type @a@.
data TypeField a where
  Basic :: BaseField a => (FieldSettings FlexForm) -> TypeField a
  SingleChoiceField
    :: Eq a
    => ChoiceShape
    -> (FieldSettings FlexForm)
    -> [(SomeMessage FlexForm, a)]
    -> TypeField a
  MultipleChoiceField
    :: Eq a
    => ChoiceShape
    -> (FieldSettings FlexForm)
    -> [(SomeMessage FlexForm, a)]
    -> TypeField (MultipleChoice a)


{- |

The layout data type.
This type is used to select and arrange the input fields in a type-safe manner.
Each `FormPiece` is a form fragment parametrized by the desired final type of the complete form
and the type of the fragment itself.
The overall type is given as a plain type,
while the type of the fragment is a type-level non-empty list of types,
corresponding to the selected input fields.

For example, for a product type

@
data Person = Person {name :: Text, age :: Int, occupation :: Text}
@

we might want to define a form fragment that contains only the first two parts of the record,
but is locked into becoming a @Person@ form:

@
nameAgePiece :: FormPiece Person (OneField Text :> OneField Int)
@

or we could leave the overall type variable, so any type consisting of `Text` and `Int` in that order,
e.g. @(Text,Int)@ could use the fragment:

@
stringIntPiece :: FormPiece t (OneField Text :> OneField Int)
@

You will mostly be able to use the simpler type synonyms `SimpleFormPiece`, `ListFormPiece`,
`AnyFormPiece` or `CompleteForm` to avoid dealing with type-level lists.
-}
data FormPiece finalType fields where
  Single :: TypeField a -> SimpleFormPiece t a
  Combine :: Alignment -> FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
  List :: Alignment -> [TypeField a] -> ListFormPiece t a


{- |
Alias for a `FormPiece` whose fields exactly match the complete form representation of its final type.
Adding another piece will therefore result in a value that is no longer a `CompleteForm`.

@
CompleteForm Person ≡ AnyFormPiece Person Person ≡ FormPiece Person (FormTypes Person)
@
-}
type CompleteForm a = AnyFormPiece a a

{- |
Alias for a `FormPiece` with exactly one type and field
that avoids having to write out the type-level list.

@
SimpleFormPiece t Text ≡ FormPiece t (OneField Text)
@
-}
type SimpleFormPiece t a = FormPiece t (OneField a)


{- |
Alias for a `FormPiece` with exactly one type but multiple fields (a collection)
that avoids having to write out the type-level list.

@
ListFormPiece t Double ≡ FormPiece t (ManyFields Double)
@
-}
type ListFormPiece t a = FormPiece t (ManyFields a)


{- |
Alias for a `FormPiece` with arbitrarily many types
that avoids having to write out the type-level list.

@
AnyFormPiece t Person ≡ FormPiece t (OneField Text :> OneField Int :> OneField Text)
@

@
AnyFormPiece t (Text,Int,Text) ≡ FormPiece t (OneField Text :> OneField Int :> OneField Text)
@

-}
type AnyFormPiece t a = FormPiece t (FormTypes a)


-- | Choice between horizontal and vertical alignment.
data Alignment = Horizontal | Vertical deriving (Eq,Show)


-- | Choice between radio buttons/checkboxes and selection menus
data ChoiceShape
  = Buttons Alignment
  -- ^ Buttons arranged according to the given alignment
  | Dropdown
  -- ^ Selection menu


{- |
Types that can be represented as a simple Yesod input field.
A `BaseField` instance of type @a@ is needed if @a@ requires a type-specific input method,
i.e. it is not just a wrapping newtype or product type that can be built out of existing input methods.

Common types, like `Int`, `Text` or `Bool`, are already instances of this class,
so you should not need to write your own instances.

An instance can be given manually using the `Field` constructor
or the `convertField` function on an existing `Field` if required anyway.

=== __Example__

>>> instance BaseField MyCoolType where baseField = convertField toCool fromCool existingField
-}
class (Formify a, FormTypes a ~ OneField a) => BaseField a where
  {- |
  The Yesod field used to render values of this type.
  The `Yesod.Form.Types.fieldParse` component of the `Field` is not used.
  It can be assigned `undefined`.
  -}
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


instance (Formify a, PathPiece a) => BaseField (Hidden a) where
  baseField = hiddenField


-- This indicates I should probably change this class to something more succinct.
-- The first function is never used, since it normally handles the parsing.
instance Show a => BaseField (SingleInputList a) where
  baseField = convertField undefined (pack . intercalate ", " . map show . getList) textField


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


{- |
Renders a form from a `CompleteForm` description,
optionally prefilling its fields with a default value.

Whether individual fields are required or optional is determined by the `CompleteForm`'s result type.
For example, a field corresponding to `Text` is required,
while one corresponding to `Maybe` `Text` is optional.
The type of the default value, when present,
must match that result type and can be used to infer it.

For example

@
formify (Just "enter name") $ basic "last name"
@

determines the `Text` input is required

@
formify (Just $ Just "enter name") $ basic "last name"
@

would make it optional instead.

This also applies for multiple fields

@
formify (Just ("enter name", True)) $ basic "last name" >- basic "receive newsletter?"
@

means both fields are required

@
formify (Just ("enter name", Nothing)) $ basic "last name" >- basic "receive newsletter?"
@

means the `Text` field is required, while the `Bool` field is optional.

Forms for custom data types will also automatically infer this
based on the types of the constructor fields.

Note that the type of the form can only be inferred if either the default is a `Just` value
or the `CompleteForm` was previously given an explicit type signature.
You will have to use `TypeApplications` on `formify` if that's not the case, e.g.

@
formify @(Text, Maybe Bool) Nothing $ basic "last name" >- basic "receive newsletter?"
@

More examples can be found under sections /Creating Form Pieces/ and /Composition and Layout/
-}
formify
  :: Formify a
  => Maybe a
  -- ^ Optional default value.
  -> CompleteForm a
  -- ^ Structure and type of the form.
  -> Rendered Widget
  -- ^ Rendered form.
formify mDefault = applyToWidget joinWidgets . formifyComponents mDefault


{- |
Like `formify`, but yields the individual sub-renders instead of a combined form.
Retains the layout structure given by the `CompleteForm` as a nested list.
Each inner list is a row in the form layout.
This can be used in custom forms to incorporate `formify`-generated forms.
-}
formifyComponents
  :: Formify a
  => Maybe a
  -- ^ Optional default value for form
  -> CompleteForm a
  -- ^ Structure and type of form
  -> Rendered [[Widget]]
  -- ^ structured sub-renders
formifyComponents mDefault = renderLayout (formDefaults mDefault)


{- |
Like `formifyComponents`, but forgets the layout information of `CompleteForm`.
Simply returns all sub-renders in a flat list.
-}
formifyComponentsFlat
  :: Formify a
  => Maybe a
  -- ^ Optional default value for form
  -> CompleteForm a
  -- ^ Structure and type of form
  -> Rendered [Widget]
  -- ^ sub-renders
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
    convertField MultipleChoice getChoices $ case k of
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


{- |
A typed single input field.
-}
basicField :: BaseField a => FieldSettings FlexForm -> TypeField a
basicField = Basic


{- |
A `TypeField` for custom enum types.
This is either a set of radio buttons or a selection menu,
depending on the given `ChoiceShape`.

The third argument is an assignment of labels for each enum constructor.
-}
singleChoiceEnumField
  :: (Eq a, Bounded a, Enum a)
  => ChoiceShape
  -> FieldSettings FlexForm
  -- ^ `FieldSettings` for the choice field.
  -> (a -> SomeMessage FlexForm)
  -- ^ Function from enum type values to labels.
  -> TypeField a
singleChoiceEnumField shape fs = SingleChoiceField shape fs . optionsFromType


{- |
A `TypeField` for the predefined `SingleChoiceSelection` type.
This is either a set of radio buttons or a selection menu,
depending on the given `ChoiceShape`.
-}
singleChoiceField
  :: ChoiceShape
  -> FieldSettings FlexForm
  -- ^ `FieldSettings` for the choice field.
  -> [SomeMessage FlexForm]
  -- ^ Option labels
  -> TypeField SingleChoiceSelection
singleChoiceField shape fs = SingleChoiceField shape fs . options


multipleChoiceField
  :: ChoiceShape
  -> FieldSettings FlexForm
  -- ^ `FieldSettings` for the choice field.
  -> [SomeMessage FlexForm]
  -- ^ Option labels
  -> TypeField MultipleChoiceSelection
multipleChoiceField shape fs = MultipleChoiceField shape fs . options


multipleChoiceEnumField
  :: (Eq a, Bounded a, Enum a)
  => ChoiceShape
  -> FieldSettings FlexForm
  -- ^ `FieldSettings` for the choice field.
  -> (a -> SomeMessage FlexForm)
  -- ^ Function from enum type values to labels.
  -> TypeField (MultipleChoice a)
multipleChoiceEnumField shape fs = MultipleChoiceField shape fs . optionsFromType


single :: TypeField a -> SimpleFormPiece t a
single = Single


{- |
A typed single input `FormPiece`.

=== __Example__

>>> printWidget "en" $ formify @Int Nothing $ basic "Age"
<div class="flex-form-div form-group">
...
    <label for="flexident1">
      Age
    </label>
    <input id="flexident1" name="flex1" type="number" step="1" required="" value="">
...
</div>

![rendered Int form piece](doc-images/basic.png)
-}
basic :: BaseField a => FieldSettings FlexForm -> SimpleFormPiece t a
basic = single . basicField


{- |
A `FormPiece` for the predefined `SingleChoiceSelection` type.
This is either a set of radio buttons or a selection menu,
depending on the given `ChoiceShape`.

=== __Example__

>>> let labels = ["First Option", "Second Option", "Third Option"]
>>> printWidget "en" $ formify (Just $ singleChoiceAnswer 3) $ singleChoice Dropdown "Choose one" labels
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

![rendered single choice field](doc-images/single_choice.png)
-}
singleChoice
  :: ChoiceShape
  -> FieldSettings FlexForm
  -- ^ `FieldSettings` for the choice field.
  -> [SomeMessage FlexForm]
  -- ^ option labels
  -> SimpleFormPiece t SingleChoiceSelection
singleChoice shape fs = single . singleChoiceField shape fs


{- |
A `FormPiece` for custom enum types.
This is either a set of radio buttons or a selection menu,
depending on the given `ChoiceShape`.

The third argument is an assignment of labels for each enum constructor.


=== __Examples__

>>> printWidget "en" $ formify (Just Two) $ singleChoiceEnum (Buttons Horizontal) "Choose one" $ showToUniversalLabel @MyType
...
<div class="flex-form-div form-group">
...
    <label for="flexident1">
      Choose one
    </label>
    <div>
      <span id="flexident1">
        <label>
          <input id="flexident1-1" type="radio" ... value="1" required...>
          One
        </label>
        <label>
          <input id="flexident1-2" type="radio" ... value="2" checked required...>
          Two
        </label>
        <label>
          <input id="flexident1-3" type="radio" ... value="3" required...>
          Three
        </label>
      </span>
    </div>
...
</div>

![rendered single choice enum field](doc-images/single_choice_enum_buttons.png)

>>> printWidget "en" $ formify (Just Two) $ singleChoiceEnum Dropdown "Choose one" $ showToUniversalLabel @MyType
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

![rendered single choice enum field 2](doc-images/single_choice_enum_dropdown.png)
-}
singleChoiceEnum
  :: (Eq a, Bounded a, Enum a)
  => ChoiceShape
  -> FieldSettings FlexForm
  -- ^ `FieldSettings` for the choice field.
  -> (a -> SomeMessage FlexForm)
  -- ^ Function from enum type values to labels
  -> SimpleFormPiece t a
singleChoiceEnum shape fs = single . singleChoiceEnumField shape fs


{- |
A `FormPiece` for the predefined `MultipleChoiceSelection` type.
This is either a set of checkboxes or a multi-selection menu,
depending on the given `ChoiceShape`.

=== __Example__

>>> let labels = ["First Option", "Second Option", "Third Option"]
>>> printWidget "en" $ formify (Just $ multipleChoiceAnswer [1,2]) $ multipleChoice Dropdown "Choose some" labels
<div class="flex-form-div form-group">
...
    <label for="flexident1">
      Choose some
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

![rendered multiple choice field](doc-images/multiple_choice_dropdown.png)
-}
multipleChoice
  :: ChoiceShape
  -> FieldSettings FlexForm
  -- ^ `FieldSettings` for the choice field.
  -> [SomeMessage FlexForm]
  -> SimpleFormPiece t MultipleChoiceSelection
multipleChoice shape fs = single . multipleChoiceField shape fs


{- |
A `FormPiece` for custom enum types.
This is either a set of checkboxes or a multi-selection menu,
depending on the given `ChoiceShape`.

The third argument is an assignment of labels for each enum constructor.

=== __Examples__

>>> let prefill = Just $ MultipleChoice [Two,Three]
>>> printWidget "en" $ formify prefill $ multipleChoiceEnum (Buttons Horizontal) "Choose" $ showToUniversalLabel @MyType
...
<div class="flex-form-div form-group">
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

![rendered multiple choice enum field](doc-images/multiple_choice_enum_buttons.png)

>>> let prefill = Just $ MultipleChoice [Two,Three]
>>> printWidget "en" $ formify prefill $ multipleChoiceEnum Dropdown "Choose some" $ showToUniversalLabel @MyType
<div class="flex-form-div form-group">
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

![rendered multiple choice enum field](doc-images/multiple_choice_enum_dropdown.png)
-}
multipleChoiceEnum
  :: (Eq a, Bounded a, Enum a)
  => ChoiceShape
  -> FieldSettings FlexForm
  -- ^ `FieldSettings` for the choice field.
  -> (a -> SomeMessage FlexForm)
  -- ^ Function from enum type values to labels
  -> SimpleFormPiece t (MultipleChoice a)
multipleChoiceEnum shape fs = single . multipleChoiceEnumField shape fs


{- |
Combines two `FormPiece`s horizontally, i.e. beside each other.

=== __Examples__

Input

@
basic \"field1\" >| basic \"field2\"
@

Renders as:

![horizontal composition](doc-images/horizontal_composition.png)

Horizontal composition does not preserve empty columns when the two pieces have different numbers of rows.
If the right-hand piece is taller than the left-hand piece,
its remaining rows will therefore appear in the leftmost position.

Input

@
list Vertical basicField ["field1_1, field1_2"] >| list Vertical basicField ["field2_1", "field2_2", field2_3]
@

will __not__ result in

![horizontal composition expectation](doc-images/horizontal_caution_wrong.png)

but instead in

![horizontal composition reality](doc-images/horizontal_caution_correct.png)
-}
infixr 5 >|
(>|) :: FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
(>|) = Combine Horizontal


{- |
A non-infix alias for `>|`
-}
beside :: FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
beside = (>|)


{- |
Combines two `FormPiece`s vertically, i.e. below each other.

=== __Examples__

Input

@
basic \"field1\" >- basic \"field2\"
@

Renders as:

![vertical composition](doc-images/vertical_composition.png)
-}
infixr 4 >-
(>-) :: FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
(>-) = Combine Vertical


{- |
A non-infix alias for `>-`
-}
above :: FormPiece t xs -> FormPiece t ys -> FormPiece t (xs ++ ys)
above = (>-)


{- |
Creates a FormPiece for a list type.
The length of the list is equal to the amount of seed values provided.

Preconditions:

  * The list of seed values may not be empty
  * When finally rendered through `formify`,
    if a default value list is given,
    then it must have the same length as the list of seed values

Otherwise you will encounter a runtime error.

=== __Example__

>>> let labels = ["Input 1", "Input 2", "Input 3"]
>>> printWidget "en" $ formify @[Double] Nothing $ list Horizontal basicField labels
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

![rendered list of fields](doc-images/list_basic.png)
-}
list
  :: Alignment
  -- ^ Alignment of the individual fields
  -> (a -> TypeField b)
  -- ^ how to build each field given an arbitrary value
  -> [a]
  -- ^ the list of values to build individual fields out of
  -> ListFormPiece t b
list align builder = List align . map builder


{- |
Same as `list`, but without using any field labels.
Per field attributes and CSS classes cannot be set with this function.
Instead, all fields share the given list of attributes.
Use `list` if individual configuration is required.

Preconditions:

  * The amount of fields is positive
  * When finally rendered through `formify`,
    if a default value list is given,
    then its length must match the amount of fields

Otherwise you will encounter a runtime error.

=== __Example__

Renders a series of four input fields of type Text
organized vertically beneath each other.
They are prefilled with the values given above,
assigned the CSS class \"helloInput\" and have no labels attached to them.

>>> let defaults = ["Hallo", "Hello", "Hola", "Ciao" :: Text]
>>> printWidget "en" $ formify (Just defaults) $ listWithoutLabels Vertical 4 basicField [("class","helloInput")]
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

![rendered list of unlabeled fields](doc-images/list_without_labels_basic.png)
-}
listWithoutLabels
  :: Alignment
  -- ^ Alignment of the individual fields
  -> Int
  -- ^ Amount of fields
  -> (FieldSettings FlexForm -> TypeField a)
  -- ^ The `TypeField` primitive to use
  -> [(Text,Text)]
  -- ^ List of attribute and value pairs (attribute "class" for classes)
  -> ListFormPiece t a
listWithoutLabels align amount req attrs =
  list align req $ replicate amount "" {fsAttrs = attrs}


{- |
Same as `list` but copies a single given `TypeField` multiple times.

Preconditions:

  * The number of copies is positive
  * When finally rendered through `formify`,
    if a default value list is given,
    then its length must match the number of copies

Otherwise you will encounter a runtime error.

=== __Example__

>>> printWidget "en" $ formify @[Int] Nothing $ listRepeatedly Vertical 3 (basicField "input")
<div class="flex-form-div form-group">
...
    <label for="flexident1">
      input
    </label>
    <input id="flexident1" ... type="number" ... value="">
...
</div>
<div class="flex-form-div form-group">
...
    <label for="flexident2">
      input
    </label>
    <input id="flexident2" ... type="number" ... value="">
...
</div>
<div class="flex-form-div form-group">
...
    <label for="flexident3">
      input
    </label>
    <input id="flexident3" ... type="number" ... value="">
...
</div>

![rendered repetition of a field](doc-images/list_repeatedly_basic.png)
-}
listRepeatedly
  :: Alignment
  -- ^ Alignment of the individual fields
  -> Int
  -- ^ How many copies
  -> TypeField a
  -- ^ The field to multiply
  -> ListFormPiece t a
listRepeatedly alignment amount = list alignment id . replicate amount


options :: [a] -> [(a, SingleChoiceSelection)]
options opts = zip opts $ map SingleChoiceSelection [1..]


optionsFromType :: (Bounded b, Enum b) => (b -> a) -> [(a, b)]
optionsFromType f = map (\x -> (f x, x)) [minBound .. maxBound]


pieceShape :: FormPiece t xs -> TypeShape xs
pieceShape Single {} = OneShape
pieceShape List {} = ManyShape
pieceShape (Combine _ x y) = appendShape (pieceShape x) $ pieceShape y
