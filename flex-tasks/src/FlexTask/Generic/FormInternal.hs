{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# language DefaultSignatures #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}

{- |
Generic `Yesod` input form generation and related utility functions.
-}

module FlexTask.Generic.FormInternal
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
  , repeatFieldInfo
  , repeatBuilderOn
  , single

    -- * Formify Convenience Functions
  , formifyInstanceBasicField
  , formifyInstanceOptionalField
  , formifyInstanceSingleChoice
  , formifyInstanceMultiChoice
  ) where


import Data.List.Extra      (intercalate, nubOrd, nubSort, singleton, uncons, unsnoc)
import Data.Maybe           (fromMaybe)
import GHC.Generics         (Generic(..), K1(..), M1(..), (:*:)(..))
import GHC.Utils.Misc       (equalLength)
import Data.Text            (Text, pack, unpack)
import Yesod

import FlexTask.FormUtil    (applyToWidget)
import FlexTask.Widgets
  ( checkboxField
  , horizontalRadioField
  , joinWidgets
  , renderForm
  )
import FlexTask.YesodConfig (FlexForm(..), Handler, Rendered, Widget)




{- |
Data type representing a prebuilt input field.
This type is used to determine the structure of a generated form.
The form is represented by a @[[FieldInfo]]@ type value.
Each FieldInfo value is an individual form element.
Inner lists represent the rows of the form.
All FieldInfo values in an inner list are rendered besides each other.
The outer list represents the columns of the form.
Inner lists are rendered below each other.

__Examples__

Input

@
[[Single \"field1\", Single \"field2\"]]
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
  | List Alignment [FieldInfo]
  | ChoicesDropdown (FieldSettings FlexForm) [SomeMessage FlexForm]
  | ChoicesButtons Alignment (FieldSettings FlexForm) [SomeMessage FlexForm]
  deriving (Show)


-- For tests; TODO: Move completely into test suite
deriving instance Show (FieldSettings FlexForm)

instance Show (SomeMessage FlexForm) where
  show m = '(': intercalate ", " (map unpack
      [ "German: " <> inLang "de"
      , "English: " <> inLang "en"
      ]
      ) ++ ")"
    where
      inLang l = renderMessage FlexForm{} [l] m


-- | Inner alignment of input field elements.
data Alignment = Horizontal | Vertical deriving (Eq,Show)


-- | Wrapper type for generating hidden fields.
newtype Hidden a = Hidden {getHidden :: a} deriving (Eq,Show)


-- | Wrapper type for lists. Use for a single field list input.
newtype SingleInputList a = SingleInputList {getList :: [a]} deriving (Eq,Show)

{- |
Generic single choice answer type.
Use if you want a 'choose one of multiple' style input
without caring about the underlying type.
-}
newtype SingleChoiceSelection = SingleChoiceSelection
  {getAnswer :: Maybe Int -- ^ Retrieve the selected option. @Nothing@ if none.
  } deriving (Show,Eq,Generic)
-- | Same as `SingleChoiceSelection`, but for multiple choice input.
newtype MultipleChoiceSelection = MultipleChoiceSelection
  { getAnswers :: [Int] -- ^ Retrieve the list of selected options. @[]@ if none.
  } deriving (Show,Eq,Generic)


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
  Direct use of this function is not recommended
  due to possible undetected invalidity of the result.
  It should only be used when writing manual instances of `Formify`.
  Use `formify` or its variants instead.
  -}
  formifyImplementation
      :: Maybe a -- ^ Optional default value for form.
      -> [[FieldInfo]] -- ^ Structure and type of form.
      -> ([[FieldInfo]], Rendered [[Widget]]) -- ^ remaining form structure and completed sub-renders.

  default formifyImplementation
      :: (Generic a, GFormify (Rep a))
      => Maybe a
      -> [[FieldInfo]]
      -> ([[FieldInfo]], Rendered [[Widget]])
  formifyImplementation mDefault = gformify $ from <$> mDefault



class GFormify f where
  gformify :: Maybe (f a) -> [[FieldInfo]] -> ([[FieldInfo]], Rendered [[Widget]])



instance (GFormify a, GFormify b) => GFormify (a :*: b) where
  gformify mDefault xs = (rightRest, renders)
    where
      (left,right) = case mDefault of
        Nothing        -> (Nothing,Nothing)
        Just (a :*: b) -> (Just a, Just b)
      (leftRest, leftRender) = gformify left xs
      (rightRest, rightRender) = gformify right rightFieldInfo
      (rightFieldInfo,renders) = case leftRest of
        ([]:xss) -> (xss, leftRender `vertically` rightRender)
        rest   -> (rest, leftRender `horizontally` rightRender)



horizontally
  :: Rendered [[a]]
  -> Rendered [[a]]
  -> Rendered [[a]]
f1 `horizontally` f2 = do
    res1 <- f1
    res2 <- f2
    pure $ do
      (names1,xss) <- res1
      (names2,yss) <- res2
      let
        (leftInit, leftLast) = fromMaybe (xss,[]) $ unsnoc xss
        (rightHead, rightTail) = fromMaybe ([],yss) $ uncons yss
      pure (nubOrd $ names1 ++ names2, leftInit ++ [leftLast ++ rightHead] ++ rightTail)



vertically
  :: Rendered [[a]]
  -> Rendered [[a]]
  -> Rendered [[a]]
f1 `vertically` f2 = do
    res1 <- f1
    res2 <- f2
    pure $ do
      (names1,xss) <- res1
      (names2,yss) <- res2
      pure (nubOrd $ names1 ++ names2, xss ++ yss)



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


instance {-# Overlappable #-} Formify a => Formify [a] where
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


__Examples__

@
formify (Nothing \@Int) [[single \"Age\"]]
@

Renders an input field with /type=number/ attribute, no default value and label /Age/.

@
formify (Just [\"Hallo\", \"Hello\", \"Hola\", \"Ciao\"]) [[listWithoutLabels Vertical 4 [(\"class\",\"helloInput\")]]]
@

Renders a series of four input fields, each for the type String
and organized vertically beneath each other.
They are prefilled with the values given above,
are assigned the Css class \"helloInput\" and have no labels attached to them.

@
formify
  (Nothing \@SingleChoiceSelection)
  [[ buttons
      \"Make your choice\"
      [ \"this one\"
      , \"or rather that one\"
      , \"I just can't decide\"
      ]
  ]]
@

Renders a radio button field with the given title and option labels attached.
No option is selected when the form is loaded.
-}
formify
  :: (Formify a)
  => Maybe a -- ^ Optional default value for form.
  -> [[FieldInfo]] -- ^ Structure of form.
  -> Rendered Widget -- ^ Rendered form.
formify = checkAndApply joinWidgets


{- |
like `formify`, but yields the individual sub-renders instead of a combined form.
Retains the layout structure given by the `FieldInfo` list argument.
This can be used in custom forms to incorporate generated inputs.
-}
formifyComponents :: Formify a => Maybe a -> [[FieldInfo]] -> Rendered [[Widget]]
formifyComponents = checkAndApply id


{- |
like `formifyComponents`, but takes a simple list of `FieldInfo` values.
The sub-renders will also be returned as a flat list without any additional structure.
-}
formifyComponentsFlat :: Formify a => Maybe a -> [FieldInfo] -> Rendered [Widget]
formifyComponentsFlat ma = checkAndApply concat ma . (:[])


checkAndApply
  :: Formify a
  => ([[Widget]] -> b)
  -> Maybe a
  -> [[FieldInfo]]
  -> Rendered b
checkAndApply toOutput ma xs = case rest of
    ([]:ns)
      | null ns   -> applyToWidget toOutput renders
    _ -> error $
      "The form generation did not use up all supplied FieldSettings values. " ++
      "Confirm your field type make sense with the amount of given FieldInfo values."
  where
    (rest, renders) = formifyImplementation ma xs


renderNextField
  :: (FieldInfo ->
       ( FieldSettings FlexForm
       , FieldSettings FlexForm -> Maybe a -> AForm Handler a
       )
     )
  -> Maybe a
  -> [[FieldInfo]]
  -> ([[FieldInfo]], Rendered [[Widget]])
renderNextField _ _ [] = error "Ran out of FieldInfo values before finishing the form!"
renderNextField h ma ((x : xs) : xss) =
  let
    (lab, g) = h x
  in
    (xs:xss, applyToWidget (singleton . singleton) $ renderForm (`g` ma) lab)
renderNextField _ _ _ = error "Incorrect FieldInfo for a field or single/multi choice!"

{- |
Premade `formifyImplementation` for types with `BaseForm` instances.
Use within manual instances of `Formify`.
-}
formifyInstanceBasicField
    :: BaseForm a
    => Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered [[Widget]])
formifyInstanceBasicField = renderNextField
  (\case
      Single fs -> (fs, areq baseForm)
      _ -> error "Incorrect FieldInfo for a basic field. Use 'single'!"
  )

{- |
Same as `formifyInstanceBasicField`, but for optional fields with `Maybe` wrapping.
-}
formifyInstanceOptionalField
    :: BaseForm a
    => Maybe (Maybe a)
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered [[Widget]])
formifyInstanceOptionalField = renderNextField
  (\case
      Single fs -> (fs, aopt baseForm)
      _ -> error "Incorrect FieldInfo for an optional basic field. Use 'single'!"
  )


formifyInstanceList
    :: (Formify a)
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered [[Widget]])
formifyInstanceList _ [] = error "Ran out of FieldInfo values before finishing the form!"
formifyInstanceList _ ((List _ [] : _) : _) = error "List used without supplying any FieldInfo values!"
formifyInstanceList mas ((List align fs : xs) : xss) =
    ( xs:xss
    , foldr1 addParams
        [snd $ formifyImplementation d [[f]] | (d,f) <- zip defaults fs]
    )
  where
    defaults = case mas of
      Nothing -> repeat Nothing
      Just ds
        | length ds /= length fs +1
          -> error $ "The default value contains too many/few individual values. " ++
                     "It does not match the amount of FieldInfo supplied."
        | otherwise
          -> sequence mas

    addParams f1 f2 = do
      res1 <- f1
      res2 <- f2
      pure $ do
        (names1,wid1) <- res1
        (names2,wid2) <- res2
        pure ([nubOrd $ concat $ names1 ++ names2]
             , case align of
                 Vertical   -> wid1 ++ wid2
                 Horizontal -> [concat (wid1 ++ wid2)])

formifyInstanceList _ _ = error "Incorrect FieldInfo for a list of fields! Use one of the list builders."



{- |
Premade `formifyImplementation` for "single choice" forms of enum types.
Use within manual instances of `Formify`.
-}
formifyInstanceSingleChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered [[Widget]])
formifyInstanceSingleChoice = renderNextSingleChoiceField zipWithEnum

renderNextSingleChoiceField
    :: Eq a
    => ([SomeMessage FlexForm] -> [(SomeMessage FlexForm, a)])
    -> Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered [[Widget]])
renderNextSingleChoiceField pairsWith =
  renderNextField
  (\case
      ChoicesDropdown fs opts -> ( fs
                                 , areq $ selectField $ withOptions opts
                                 )
      ChoicesButtons align fs opts -> ( fs
                                      , areq $
                                          case align of
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
    -> ([[FieldInfo]], Rendered [[Widget]])
renderNextMultipleChoiceField pairsWith =
  renderNextField
  (\case
      ChoicesDropdown fs opts -> ( fs
                                 , areq $ multiSelectField $ withOptions opts
                                 )
      ChoicesButtons align fs opts -> ( fs
                                      , areq $
                                          case align of
                                            Vertical   -> checkboxField True
                                            Horizontal -> checkboxField False
                                          $ withOptions opts
                                      )
      _ -> error "Incorrect FieldInfo for a multiple choice field! Use one of the 'buttons' or 'dropdown' functions."
  )
  where withOptions = optionsPairs . pairsWith



-- | Same as `formifyInstanceSingleChoice`, but for multiple choice.
formifyInstanceMultiChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered [[Widget]])
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
Use with SingleChoiceSelection or MultipleChoiceSelection.
__Usage with a custom enum type is not recommended due to error proneness.__
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
Use with SingleChoiceSelection or MultipleChoiceSelection.
__Usage with a custom enum types is not recommended due to error proneness.__
-}
dropdown
  :: FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [SomeMessage FlexForm]  -- ^ Option labels
  -> FieldInfo
dropdown = ChoicesDropdown



{- |
Create FieldInfo for a number of basic fields.
Their result will be handled as a list of values.
Use for lists of BaseForm fields like `Int`, `String`, `Double`.
-}
list
  :: Alignment
  -> [FieldSettings FlexForm] -- ^ FieldSettings of individual fields
  -> FieldInfo
list align = repeatBuilderOn align single


{- |
Same as `list`, but without using any field labels.
Attributes and CSS classes for each field cannot be set with this function.
Instead, all fields share the given list of attributes.
Use `list` if individual configuration is required.
-}
listWithoutLabels
  :: Alignment
  -> Int           -- ^ Amount of fields
  -> [(Text,Text)] -- ^ List of attribute and value pairs (attribute "class" for classes)
  -> FieldInfo
listWithoutLabels align amount attrs = List align $ replicate amount $ single "" {fsAttrs = attrs}


{- |
Create FieldInfo for a number of arbitrary fields.
Takes the builder to repeatedly use for each field
and a list of values to use it on.
Their result will be handled as a list of values.
Use to render lists of dropdown or button fields with different labels.
-}
repeatBuilderOn
  :: Alignment
  -> (a -> FieldInfo) -- ^ FieldInfo builder to use
  -> [a]              -- ^ List of values to use builder on
  -> FieldInfo
repeatBuilderOn align builder = List align . map builder


{- |
Create FieldInfo for a list containing exact copies the specified field.
The results of the copies will be handled as a list of values.
Use to render lists of dropdown or button fields with identical labels.
-}
repeatFieldInfo
  :: Alignment
  -> Int       -- ^ How many copies
  -> FieldInfo -- ^ The field to multiply
  -> FieldInfo
repeatFieldInfo alignment amount = List alignment . replicate amount



-- | Create FieldInfo for a standalone field.
single :: FieldSettings FlexForm -> FieldInfo
single = Single
