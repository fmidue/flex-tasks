{-# OPTIONS_GHC -Wno-orphans #-}
{-# language DefaultSignatures #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
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


import Data.List.Extra      (nubSort, uncons, unsnoc)
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
import FlexTask.YesodConfig (FlexForm, Handler, Rendered, Widget)




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
  | List Alignment [FieldSettings FlexForm]
  | ChoicesDropdown (FieldSettings FlexForm) [Text]
  | ChoicesButtons Alignment (FieldSettings FlexForm) [Text]
  | InternalListElem (FieldSettings FlexForm)
  deriving (Show)


-- For tests; Not used in actual code
instance Show (FieldSettings FlexForm) where
  show (FieldSettings _ _ fId fName fAttrs) = unlines
    [ "Id: " ++ show fId
    , "Name: " ++ show fName
    , "Attributes: " ++ show fAttrs
    ]


-- | Inner alignment of input field elements.
data Alignment = Horizontal | Vertical deriving (Eq,Show)


-- | Wrapper type for generating hidden fields.
newtype Hidden a = Hidden {getHidden :: a} deriving (Eq,Show)


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
      | otherwise -> error "Mismatched amount of field names and actual fields!"
    _             -> error "Impossible: Formify failed to assemble the form."
  where
    (rest, renders) = formifyImplementation ma xs


renderNextField
  :: (FieldInfo ->
       ( FieldSettings FlexForm
       , Bool
       , FieldSettings FlexForm -> Maybe a -> AForm Handler a
       )
     )
  -> Maybe a
  -> [[FieldInfo]]
  -> ([[FieldInfo]], [[Rendered Widget]])
renderNextField _ _ [] = error "Incorrect amount of field names"
renderNextField h ma ((x : xs) : xss) =
  let
    (lab, newId, g) = h x
  in
    (xs:xss, [[renderForm newId (`g` ma) lab]])
renderNextField _ _ _ = error "Incorrect naming scheme for a field or single/multi choice!"

{- |
Premade `formifyImplementation` for types with `BaseForm` instances.
Use within manual instances of `Formify`.
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
      _ -> error "Internal mismatch of FieldInfo and rendering function"
  )

{- |
Same as `formifyInstanceBasicField`, but for optional fields with `Maybe` wrapping.
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
      _ -> error "Internal mismatch of FieldInfo and rendering function"
  )


formifyInstanceList
    :: (Formify a)
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
formifyInstanceList _ [] = error "ran out of field names"
formifyInstanceList _ ((List _ [] : _) : _) = error "List of fields without names!"
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
          -> error "Not enough values in the default list!"
        | otherwise
          -> sequence mas

    headError [] = error "Defaults should never be empty here!"
    headError (x:_) = x

    firstRender = snd $ formifyImplementation (headError defaults) [[single f]]
    renderRest def fSettings = formifyImplementation def [[InternalListElem fSettings]]
    followingRenders = concat [snd $ renderRest d fSet | (d,fSet) <- zip (drop 1 defaults) fs]

formifyInstanceList _ _ = error "Incorrect naming scheme for a list of fields!"



{- |
Premade `formifyImplementation` for "single choice" forms of enum types.
Use within manual instances of `Formify`.
-}
formifyInstanceSingleChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
formifyInstanceSingleChoice = renderNextSingleChoiceField zipWithEnum

renderNextSingleChoiceField
    :: Eq a
    => ([Text] -> [(Text, a)])
    -> Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
renderNextSingleChoiceField pairsWith =
  renderNextField
  (\case
      ChoicesDropdown fs opts -> ( fs
                                 , True
                                 , areq $ selectField $ withOptions opts
                                 )
      ChoicesButtons align fs opts -> ( fs
                                      , True
                                      , areq $
                                          case align of
                                            Vertical -> radioField
                                            Horizontal -> horizontalRadioField
                                          $ withOptions opts
                                      )
      _ -> error "Incorrect naming scheme for a single choice!"
  )
  where withOptions = optionsPairs . pairsWith

renderNextMultipleChoiceField
    :: Eq a
    => ([Text] -> [(Text, a)])
    -> Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
renderNextMultipleChoiceField pairsWith =
  renderNextField
  (\case
      ChoicesDropdown fs opts -> ( fs
                                 , True
                                 , areq $ multiSelectField $ withOptions opts
                                 )
      ChoicesButtons align fs opts -> ( fs
                                      , True
                                      , areq $
                                          case align of
                                            Vertical   -> checkboxField True
                                            Horizontal -> checkboxField False
                                          $ withOptions opts
                                      )
      _ -> error "Incorrect naming scheme for a multi choice!"
  )
  where withOptions = optionsPairs . pairsWith



-- | Same as `formifyInstanceSingleChoice`, but for multiple choice.
formifyInstanceMultiChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], [[Rendered Widget]])
formifyInstanceMultiChoice = renderNextMultipleChoiceField zipWithEnum



zipWithEnum :: forall a. (Bounded a, Enum a) => [Text] -> [(Text, a)]
zipWithEnum labels
  | equalLength labels options = zip labels options
  | otherwise = error "Labels list and options list are of different lengths in an Enum choice form."
  where options = [minBound .. maxBound :: a]






{- |
Same as `buttons`, but using an explicit enum type.
-}
buttonsEnum
  :: (Bounded a, Enum a)
  => Alignment
  -> FieldSettings FlexForm -- ^ FieldSettings for option input
  -> (a -> Text)            -- ^ Function from enum type values to labels.
  -> FieldInfo
buttonsEnum align t f = ChoicesButtons align t $ map f [minBound .. maxBound]



{- |
Create FieldInfo for a button field.
Will turn into either radio buttons or checkboxes
depending on the form type.
-}
buttons
  :: Alignment
  -> FieldSettings FlexForm -- ^ FieldSettings for option input
  -> [Text]                 -- ^ Option labels
  -> FieldInfo
buttons = ChoicesButtons



{- |
Same as `dropdown`, but using an explicit enum type.
-}
dropdownEnum
  :: (Bounded a, Enum a)
  => FieldSettings FlexForm -- ^ FieldSettings for select input
  -> (a -> Text)            -- ^ Function from enum type values to labels.
  -> FieldInfo
dropdownEnum t f = ChoicesDropdown t $ map f [minBound .. maxBound]



{- |
Create FieldInfo for a dropdown menu field.
Will turn into either single or multiple selection field
depending on the form type.
-}
dropdown
  :: FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [Text]                  -- ^ Option labels
  -> FieldInfo
dropdown = ChoicesDropdown



{- |
Create FieldInfo for a number of fields.
Their result will be handled as a list of values.
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
-}
listWithoutLabels
  :: Alignment
  -> Int           -- ^ Amount of fields
  -> [(Text,Text)] -- ^ List of attribute and value pairs (attribute "class" for classes)
  -> FieldInfo
listWithoutLabels align amount attrs = List align $ replicate amount $ "" {fsAttrs = attrs}



-- | Create FieldInfo for a standalone field.
single :: FieldSettings FlexForm -> FieldInfo
single = Single
