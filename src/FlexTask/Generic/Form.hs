{-# language DefaultSignatures #-}
{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}
{-# language LambdaCase #-}

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

    -- * Type Classes
  , BaseForm(..)
  , Formify(..)
  , formify
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
  , formifyInstanceSingleChoice
  , formifyInstanceMultiChoice
  ) where


import Data.List.Extra      (nubSort)
import GHC.Generics         (Generic(..), K1(..), M1(..), (:*:)(..))
import GHC.Utils.Misc       (equalLength)
import Data.Text            (Text, pack, unpack)
import Yesod

import FlexTask.FormUtil    (($$>))
import FlexTask.Widgets
  ( horizontalRadioField
  , renderFlatOrBreak
  , verticalCheckboxesField
  )
import FlexTask.YesodConfig (FlexForm, Handler, Rendered)




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
[[listWithoutLabels Vertical 2],[listWithoutLabels Vertical 2]]
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
  = Single Text
  | List Alignment [Text]
  | ChoicesDropdown Text [Text]
  | ChoicesButtons Alignment Text [Text]
  | InternalListElem Text
  deriving (Eq,Show)


-- | Inner alignment of input field elements.
data Alignment = Horizontal | Vertical deriving (Eq,Show)


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


instance BaseForm Bool where
  baseForm = boolField


instance BaseForm Double where
  baseForm = doubleField



{- |
Class for generic generation of Html input forms for a given type.
Bodyless instances can be declared for any type instancing Generic.
__Exception: Types with multiple constructors.__
Use utility functions for those or provide your own instance.
-}
class Formify a where
  formifyImplementation
      :: Maybe a -- ^ Optional default value for form.
      -> [[FieldInfo]] -- ^ Structure and type of form.
      -> ([[FieldInfo]], Rendered) -- ^ remaining structure and rendered form.

  default formifyImplementation
      :: (Generic a, GFormify (Rep a))
      => Maybe a
      -> [[FieldInfo]]
      -> ([[FieldInfo]], Rendered)
  formifyImplementation mDefault = gformify $ from <$> mDefault



class GFormify f where
  gformify :: Maybe (f a) -> [[FieldInfo]] -> ([[FieldInfo]], Rendered)



instance (GFormify a, GFormify b) => GFormify (a :*: b) where
  gformify mDefault xs = (rightRest, leftRender $$> rightRender)
    where
      (left,right) = case mDefault of
        Nothing        -> (Nothing,Nothing)
        Just (a :*: b) -> (Just a, Just b)
      (leftRest, leftRender) = gformify left xs
      (rightRest, rightRender) = gformify right leftRest




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


instance Formify Bool where
  formifyImplementation = formifyInstanceBasicField



instance Formify Double where
  formifyImplementation = formifyInstanceBasicField


instance (Formify a, Formify b) => Formify (a,b)

instance (Formify a, Formify b, Formify c) => Formify (a,b,c)

instance (Formify a, Formify b, Formify c, Formify d) => Formify (a,b,c,d)


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
Alternative to `formifyImplementation`
that only produces the rendered form.
Will fail if remaining form structure is not empty,
indicating the form is finished or faulty.

This is the main way to build generic forms.
Use in conjunction with `FieldInfo` builders to generate a form.

__Examples__

@
formify (Nothing \@Int) [[single \"Age\"]]
@

Renders an input field with /type=number/ attribute, no default value and label /Age/.

@
formify (Just [\"Hallo\", \"Hello\", \"Hola\", \"Ciao\"]) [[listWithoutLabels Vertical]]
@

Renders a series of four input fields, each for the type String
and organized vertically beneath each other.
They are prefilled with the values given above and have no labels attached to them.

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
  -> Rendered -- ^ Rendered form.
formify ma xs
      | null names = form
      | otherwise  = error "mismatched amount of field names and actual fields!"
    where
      (names, form) = formifyImplementation ma xs



renderNextField
  :: (FieldInfo ->
       ( Text
       , Bool
       , FieldSettings FlexForm -> Maybe a -> AForm Handler a
       )
     )
  -> Maybe a
  -> [[FieldInfo]]
  -> ([[FieldInfo]], Rendered)
renderNextField _ _ [] = error "Incorrect amount of field names"
renderNextField h ma ((x : xs) : xss) =
  let
    (lab, newId, g) = h x
  in
    if null xs
    then (xss, renderFlatOrBreak True newId (`g` ma) lab)
    else (xs:xss, renderFlatOrBreak False newId (`g` ma) lab)
renderNextField _ _ _ = error "Incorrect naming scheme for a field or single/multi choice!"

formifyInstanceBasicField
    :: BaseForm a
    => Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceBasicField = renderNextField
  (\case
      Single t -> (t, True, areq baseForm)
      InternalListElem t -> (t, False, areq baseForm)
      _ -> error "Internal mismatch of FieldInfo and rendering function"
  )

formifyInstanceOptionalField
    :: BaseForm a
    => Maybe (Maybe a)
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceOptionalField = renderNextField
  (\case
      Single t -> (t, True, aopt baseForm)
      InternalListElem t -> (t, False, aopt baseForm)
      _ -> error "Internal mismatch of FieldInfo and rendering function"
  )


formifyInstanceList
    :: (Formify a)
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceList _ [] = error "ran out of field names"
formifyInstanceList _ ((List _ [] : _) : _) = error "List of fields without names!"
formifyInstanceList mas ((List align (f:fs) : xs) : xss) =
    (rest, snd <$> foldr joinThem zero theList)
  where
    lastInRow = null xs
    amount = length fs +1

    (rest,theList,stopCondition) = case align of
      Horizontal
        | lastInRow -> ( xss, [[Single f : [InternalListElem lab | lab <- fs]]], null)
        | otherwise -> ( xs:xss
                       , [[Single f : [InternalListElem lab | lab <- fs ++ [undefined]]]]
                       , any $ (<= 1) . length
                       )

      Vertical
        | lastInRow -> ( xss, [[Single f]] : [[[InternalListElem lab]]| lab <- fs], null)
        | otherwise -> ( xs:xss
                       , [[Single f]] : [[InternalListElem lab : [undefined | last fs == lab]] | lab <- fs]
                       , \ds -> null ds || length (last ds) <= 1
                       )

    sequencedDefaults = case mas of
      Nothing -> replicate amount Nothing
      Just ds -> if length ds /= amount
                   then error "Not enough values in the default list!"
                   else sequence mas

    defOrder = case align of
      Horizontal -> sequencedDefaults
      Vertical   -> reverse sequencedDefaults

    zero = pure (defOrder, pure ([],pure ()))

    joinThem m mList = do
      (defs,inner) <- mList
      let (remain,render) = formifyImplementation (head defs) m
      new <- render
      old <- if stopCondition remain
               then pure inner
               else snd <$> joinThem remain (pure (tail defs, inner))
      pure ( tail defs
           , do
               (t,w) <- new
               (_,accumW) <- old
               pure (t,w >> accumW)
           )
formifyInstanceList _ _ = error "Incorrect naming scheme for a list of fields!"



{- |
Premade `formifyImplementation` for "single choice" forms of enum types.
Use within manual instances of `Formify`.
-}
formifyInstanceSingleChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceSingleChoice = renderNextSingleChoiceField zipWithEnum

renderNextSingleChoiceField
    :: Eq a
    => ([Text] -> [(Text, a)])
    -> Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
renderNextSingleChoiceField pairsWith =
  renderNextField
  (\case
      ChoicesDropdown t opts -> ( t
                                , True
                                , areq $ selectField $ withOptions opts
                                )
      ChoicesButtons align t opts -> ( t
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
    -> ([[FieldInfo]], Rendered)
renderNextMultipleChoiceField pairsWith =
  renderNextField
  (\case
      ChoicesDropdown t opts -> ( t
                                , True
                                , areq $ multiSelectField $ withOptions opts
                                )
      ChoicesButtons align t opts -> ( t
                                     , True
                                     , areq $
                                         case align of
                                           Vertical -> verticalCheckboxesField
                                           Horizontal -> checkboxesField
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
    -> ([[FieldInfo]], Rendered)
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
  -> Text        -- ^ Field title label
  -> (a -> Text) -- ^ Function from enum type values to labels.
  -> FieldInfo
buttonsEnum align t f = ChoicesButtons align t $ map f [minBound .. maxBound]



{- |
Create FieldInfo for a button field.
Will turn into either radio buttons or checkboxes
depending on the form type.
-}
buttons
  :: Alignment
  -> Text   -- ^ Field title label
  -> [Text] -- ^ Option labels
  -> FieldInfo
buttons = ChoicesButtons



{- |
Same as `dropdown`, but using an explicit enum type.
-}
dropdownEnum
  :: (Bounded a, Enum a)
  => Text        -- ^ Field title label
  -> (a -> Text) -- ^ Function from enum type values to labels.
  -> FieldInfo
dropdownEnum t f = ChoicesDropdown t $ map f [minBound .. maxBound]



{- |
Create FieldInfo for a dropdown menu field.
Will turn into either single or multiple selection field
depending on the form type.
-}
dropdown
  :: Text   -- ^ Field title label
  -> [Text] -- ^ Option labels
  -> FieldInfo
dropdown = ChoicesDropdown



{- |
Create FieldInfo for a number of fields.
Their result will be handled as a list of values.
-}
list
  :: Alignment
  -> [Text] -- ^ Labels of individual fields
  -> FieldInfo
list = List



-- | Same as `list`, but without using any field labels.
listWithoutLabels
  :: Alignment
  -> Int -- ^ Amount of fields
  -> FieldInfo
listWithoutLabels align amount = List align $ replicate amount ""



-- | Create FieldInfo for a standalone field.
single
  :: Text -- ^ Label
  -> FieldInfo
single = Single
