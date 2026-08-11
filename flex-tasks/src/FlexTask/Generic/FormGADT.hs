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

module FlexTask.Generic.FormGADT (
  TypeField,
  Requiredness,
  FormPiece,
  CompositeFormPiece,
  SimpleFormPiece,
  CompleteForm,
  Alignment(..),
  ChoiceShape(..),
  formify,
  BaseForm(..),
  Formify(..),
  basic,
  singleChoice,
  singleChoiceEnum,
  multipleChoice,
  multipleChoiceEnum,
  required,
  optional,
  single,
  list,
  listWithoutLabels,
  (>|),
  (>-),
  repeatBuilderOn,
  repeatFieldInfo,
  SingleChoiceSelection,
  MultipleChoiceSelection,
  getAnswer,
  getAnswerAsIndex,
  getAnswers,
  getAnswersAsIndices,
  singleChoiceAnswer,
  singleChoiceEmpty,
  multipleChoiceAnswer,
  multipleChoiceEmpty,
  ) where


import Yesod (
  AForm,
  Field,
  FieldSettings(fsAttrs),
  SomeMessage,
  Textarea,
  aopt,
  areq,
  boolField,
  convertField,
  doubleField,
  intField,
  multiSelectField,
  optionsPairs,
  textareaField,
  textField,
  )
import FlexTask.YesodConfig             (FlexForm, Handler, Rendered, Widget)
import Data.Kind                        (Constraint, Type)
import Data.List.Extra (
  nubOrd,
  nubSort,
  singleton,
  zipWithLongest,
  )
import Data.Maybe                       (catMaybes)
import Data.Text                        (Text, pack, unpack)
import Data.Tuple.Extra                 (first)
import FlexTask.Widgets (
  checkboxField,
  joinWidgets,
  radioField,
  renderForm,
  selectField,
  )
import FlexTask.FormUtil                (applyToWidget)
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



type CompleteForm a = FormLayout a (FormTypes a)

data Alignment = Horizontal | Vertical

data ChoiceShape = Buttons Alignment | Dropdown

data TypeField a where
  Basic :: BaseForm a => (FieldSettings FlexForm) -> TypeField a
  SingleChoice :: Eq a => ChoiceShape -> (FieldSettings FlexForm) -> [(SomeMessage FlexForm, a)] -> TypeField a
  MultipleChoice :: Eq a => ChoiceShape -> (FieldSettings FlexForm) -> [(SomeMessage FlexForm, a)] -> TypeField [a]

data Requiredness a where
  Required :: TypeField a -> Requiredness a
  Optional :: TypeField a -> Requiredness (Maybe a)


newtype SingleChoiceSelection = SingleChoiceSelection
  { getAnswer :: Int
  } deriving (Show,Eq,Generic)

getAnswerAsIndex :: SingleChoiceSelection -> Int
getAnswerAsIndex = subtract 1 . getAnswer


type MultipleChoiceSelection = [SingleChoiceSelection]


getAnswers :: MultipleChoiceSelection -> [Int]
getAnswers = map getAnswer

getAnswersAsIndices :: MultipleChoiceSelection -> [Int]
getAnswersAsIndices = map (subtract 1) . getAnswers

singleChoiceEmpty :: SingleChoiceSelection
singleChoiceEmpty = singleChoiceAnswer 0

singleChoiceAnswer :: Int -> SingleChoiceSelection
singleChoiceAnswer = SingleChoiceSelection

multipleChoiceEmpty :: MultipleChoiceSelection
multipleChoiceEmpty = []

multipleChoiceAnswer :: [Int] -> MultipleChoiceSelection
multipleChoiceAnswer = map singleChoiceAnswer . nubSort


type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data TypeList xs where
  TEmpty :: TypeList '[]
  TCons :: x -> TypeList xs -> TypeList (x ': xs)


class SplitOff xs ys where
  splitTypeList :: TypeList (xs ++ ys) -> (TypeList xs, TypeList ys)


instance SplitOff '[] ys where
  splitTypeList = (TEmpty,)

instance SplitOff xs ys => SplitOff (x ': xs) ys where
  splitTypeList (TCons x rest) = first (TCons x) $ splitTypeList rest


appendTypeList :: TypeList xs -> TypeList ys -> TypeList (xs ++ ys)
appendTypeList TEmpty = id
appendTypeList (TCons x xs) = TCons x . appendTypeList xs


data FormLayout finalType fields where
  Single :: Requiredness a -> FormLayout t '[a]
  Beside :: SplitOff xs ys => FormLayout t xs -> FormLayout t ys -> FormLayout t (xs ++ ys)
  Above :: SplitOff xs ys => FormLayout t xs -> FormLayout t ys -> FormLayout t (xs ++ ys)
  List :: Alignment -> [Requiredness a] -> FormLayout t '[[a]]


type FormPiece fields = forall model. FormLayout model fields

type SimpleFormPiece a = FormPiece '[a]
type CompositeFormPiece a = FormPiece (FormTypes a)


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


singleFormDefaults :: a -> TypeList '[a]
singleFormDefaults x = TCons x TEmpty


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


instance Formify (Maybe a) where
  type FormTypes (Maybe a) = '[Maybe a]
  formDefaults = singleFormDefaults


instance Formify SingleChoiceSelection where
  type FormTypes SingleChoiceSelection = '[SingleChoiceSelection]
  formDefaults = singleFormDefaults


instance {-# Overlappable #-} Formify [a] where
  type FormTypes [a] = '[[a]]
  formDefaults = singleFormDefaults


instance (Formify a, Formify b) => Formify (a,b)

instance (Formify a, Formify b, Formify c) => Formify (a,b,c)


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


splitMaybeDefaults :: SplitOff xs ys => Maybe (TypeList (xs ++ ys)) -> (Maybe (TypeList xs), Maybe (TypeList ys))
splitMaybeDefaults Nothing = (Nothing, Nothing)
splitMaybeDefaults (Just xs) = (Just left, Just right)
  where (left, right) = splitTypeList xs


formify
  :: Formify a
  => Maybe a -- ^ Optional default value for form.
  -> CompleteForm a -- ^ Structure of form.
  -> Rendered Widget -- ^ Rendered form.
formify mDefault = applyToWidget joinWidgets . formifyImplementation mDefault


basic :: BaseForm a => FieldSettings FlexForm -> TypeField a
basic = Basic



singleChoice
  :: ChoiceShape
  -> FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [SomeMessage FlexForm]  -- ^ Option labels
  -> TypeField SingleChoiceSelection
singleChoice shape fs = SingleChoice shape fs . options


singleChoiceEnum
  :: (Eq a, Bounded a, Enum a)
  => ChoiceShape
  -> FieldSettings FlexForm      -- ^ FieldSettings for select input
  -> (a -> SomeMessage FlexForm) -- ^ Function from enum type values to labels.
  -> TypeField a
singleChoiceEnum shape fs = SingleChoice shape fs . optionsFromType


multipleChoice
  :: ChoiceShape
  -> FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [SomeMessage FlexForm]  -- ^ Option labels
  -> TypeField MultipleChoiceSelection
multipleChoice shape fs = MultipleChoice shape fs . options


multipleChoiceEnum
  :: (Eq a, Bounded a, Enum a)
  => ChoiceShape
  -> FieldSettings FlexForm      -- ^ FieldSettings for select input
  -> (a -> SomeMessage FlexForm) -- ^ Function from enum type values to labels.
  -> TypeField [a]
multipleChoiceEnum shape fs = MultipleChoice shape fs . optionsFromType


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
        , zipWithLongest (\xs ys -> concat $ catMaybes [xs,ys]) xss yss
        )



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


required :: TypeField a -> Requiredness a
required = Required

optional :: TypeField a -> Requiredness (Maybe a)
optional = Optional

single :: Requiredness a -> SimpleFormPiece a
single = Single


infixl 5 >|

infixl 4 >-

(>|) :: SplitOff xs ys => FormPiece xs -> FormPiece ys -> FormPiece (xs ++ ys)
(>|) = Beside

(>-) :: SplitOff xs ys => FormPiece xs -> FormPiece ys -> FormPiece (xs ++ ys)
(>-) = Above


list
  :: Alignment
  -> (FieldSettings FlexForm -> Requiredness a)
  -> [FieldSettings FlexForm] -- ^ FieldSettings of individual fields
  -> SimpleFormPiece [a]
list = repeatBuilderOn


listWithoutLabels
  :: Alignment
  -> Int           -- ^ Amount of fields
  -> (FieldSettings FlexForm -> Requiredness a)
  -> [(Text,Text)] -- ^ List of attribute and value pairs (attribute "class" for classes)
  -> SimpleFormPiece [a]
listWithoutLabels align amount req attrs =
  list align req $ replicate amount "" {fsAttrs = attrs}


repeatBuilderOn
  :: Alignment
  -> (a -> Requiredness b) -- ^ FieldInfo builder to use
  -> [a]        -- ^ List of values to use builder on
  -> SimpleFormPiece [b]
repeatBuilderOn align builder = List align . map builder


repeatFieldInfo
  :: Alignment
  -> Int       -- ^ How many copies
  -> Requiredness a -- ^ The field to multiply
  -> SimpleFormPiece [a]
repeatFieldInfo alignment amount = repeatBuilderOn alignment id . replicate amount


options :: [a] -> [(a, SingleChoiceSelection)]
options opts = zip opts $ map SingleChoiceSelection [1..]


optionsFromType :: (Bounded b, Enum b) => (b -> a) -> [(a, b)]
optionsFromType f = map (\x -> (f x, x)) [minBound .. maxBound]
