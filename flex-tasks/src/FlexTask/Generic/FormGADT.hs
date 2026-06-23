{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DeriveGeneric #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module FlexTask.Generic.FormGADT where

import Yesod                            hiding (selectField, radioField)
import FlexTask.YesodConfig
import Data.Kind                        (Type)
import Data.List.NonEmpty               (NonEmpty, toList)
import Data.List.Extra (
  intercalate,
  nubOrd,
  nubSort,
  singleton,
  uncons,
  unsnoc,
  )
import Data.Maybe                       (fromMaybe)
import Data.Text                        (Text, pack, unpack)
import Data.Tuple.Extra                 (first)
import FlexTask.Widgets (
  renderForm,
  selectField,
  radioField,
  checkboxField,
  joinWidgets,
  )
import FlexTask.FormUtil                (applyToWidget)
import GHC.Generics (
  Generic(..),
  (:+:),
  (:*:)(..),
  K1(unK1),
  M1(unM1),
  U1,
  )
import GHC.TypeLits                     (ErrorMessage((:<>:), Text), TypeError)
import GHC.Utils.Misc                   (equalLength)


data Alignment = Horizontal | Vertical deriving (Eq,Show)

data Kind = Buttons Alignment | Dropdown deriving (Eq,Show)

data FieldInfo a where
  Basic :: BaseForm a => (FieldSettings FlexForm) -> FieldInfo a
  SingleChoice :: Eq a => Kind -> (FieldSettings FlexForm) -> [(SomeMessage FlexForm, a)] -> FieldInfo a
  MultiChoice :: Eq a => Kind -> (FieldSettings FlexForm) -> [(SomeMessage FlexForm, a)] -> FieldInfo [a]

data Requiredness a where
  Required :: FieldInfo a -> Requiredness a
  Optional :: FieldInfo a -> Requiredness (Maybe a)


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

deriving instance Show (FieldSettings FlexForm)

instance Show (SomeMessage FlexForm) where
  show m = '(': intercalate ", "
      [ "German: " <> inLang "de"
      , "English: " <> inLang "en"
      ]
      ++ ")"
    where
      inLang l = show $ renderMessage FlexForm{} [l] m


newtype FormSpec a =
  FormSpec
    { getFormSpec :: FormLayout (FormType a)
    }


type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data TypeList xs where
  TEmpty :: TypeList '[]
  TCons :: x -> TypeList xs -> TypeList (x ': xs)


class Split xs ys where
  splitTypeList :: TypeList (xs ++ ys) -> (TypeList xs, TypeList ys)


instance Split '[] ys where
  splitTypeList = (TEmpty,)

instance Split xs ys => Split (x ': xs) ys where
  splitTypeList (TCons x rest) = first (TCons x) $ splitTypeList rest


appendTypeList :: TypeList xs -> TypeList ys -> TypeList (xs ++ ys)
appendTypeList TEmpty = id
appendTypeList (TCons x xs) = TCons x . appendTypeList xs


data FormLayout fields where
  Single :: Requiredness a -> FormLayout '[a]
  Beside :: Split xs ys => FormLayout xs -> FormLayout ys -> FormLayout (xs ++ ys)
  Above :: Split xs ys => FormLayout xs -> FormLayout ys -> FormLayout (xs ++ ys)
  List :: Alignment -> NonEmpty (Requiredness a) -> FormLayout '[[a]]


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
singleFormDefaults x =
  TCons x TEmpty


class Formify a where

  type FormType a :: [Type]
  type FormType a = GFormType (Rep a)

  formDefaults :: a -> TypeList (FormType a)

  default formDefaults
    :: ( Generic a
       , GToTypeList (Rep a)
       , FormType a ~ GFormType (Rep a)
       )
    => a
    -> TypeList (FormType a)
  formDefaults =
    gToTypeList . from

  formifyImplementation
      :: Maybe a -- ^ Optional default value for form.
      -> FormSpec a -- ^ Structure and type of form.
      -> Rendered [[Widget]] -- ^ remaining form structure and completed sub-renders.
  formifyImplementation mDefault (FormSpec layout) =
    renderLayout (formDefaults <$> mDefault) layout


type family GFormType f :: [Type] where
  GFormType (M1 i c f) =
    GFormType f

  GFormType (K1 i a) =
    FormType a

  GFormType (left :*: right) =
    GFormType left ++ GFormType right


class GToTypeList f where
  gToTypeList :: f p -> TypeList (GFormType f)


instance GToTypeList f => GToTypeList (M1 i c f) where
  gToTypeList = gToTypeList . unM1


instance Formify a => GToTypeList (K1 i a) where
  gToTypeList = formDefaults . unK1


instance (GToTypeList a, GToTypeList b) => GToTypeList (a :*: b) where
  gToTypeList (left :*: right) =
    appendTypeList (gToTypeList left) $ gToTypeList right


instance TypeError
      ( 'Text "Generic Formify does not support constant constructors." :<>:
        'Text "Consider adding a manual instance instead."
      ) => GToTypeList U1 where
  gToTypeList = undefined

instance TypeError
      ( 'Text "Generic Formify does not support sum types. " :<>:
        'Text "Consider adding a manual instance instead."
      ) => GToTypeList (a :+: b) where
  gToTypeList = undefined


instance Formify Integer where
  type FormType Integer = '[Integer]
  formDefaults = singleFormDefaults


instance Formify Int where
  type FormType Int = '[Int]
  formDefaults = singleFormDefaults

instance Formify Text where
  type FormType Text = '[Text]
  formDefaults = singleFormDefaults

instance Formify String where
  type FormType String = '[String]
  formDefaults = singleFormDefaults


instance Formify Textarea where
  type FormType Textarea = '[Textarea]
  formDefaults = singleFormDefaults


instance Formify Bool where
  type FormType Bool = '[Bool]
  formDefaults = singleFormDefaults



instance Formify Double where
  type FormType Double = '[Double]
  formDefaults = singleFormDefaults


instance Formify (Maybe a) where
  type FormType (Maybe a) = '[Maybe a]
  formDefaults = singleFormDefaults


instance Formify SingleChoiceSelection where
  type FormType SingleChoiceSelection = '[SingleChoiceSelection]
  formDefaults = singleFormDefaults


instance Formify MultipleChoiceSelection where
  type FormType MultipleChoiceSelection = '[MultipleChoiceSelection]
  formDefaults = singleFormDefaults


instance {-# Overlappable #-} Formify a => Formify [a] where
  type FormType [a] = '[[a]]
  formDefaults = singleFormDefaults


instance (Formify a, Formify b) => Formify (a,b)

instance (Formify a, Formify b, Formify c) => Formify (a,b,c)


renderRequiredness :: Maybe a -> Requiredness a -> Rendered Widget
renderRequiredness mDefault (Optional field) = renderField (\f fs -> aopt f fs mDefault) field
renderRequiredness mDefault (Required field) = renderField (\f fs -> areq f fs mDefault) field


renderField :: (Field Handler a  -> FieldSettings FlexForm -> AForm Handler c) -> FieldInfo a -> Rendered Widget
renderField req info = case info of
  Basic fs -> renderForm (req baseForm) fs
  SingleChoice k fs xs -> renderForm (req $ case k of
    Dropdown -> selectField $ optionsPairs xs
    Buttons Vertical -> radioField True $ optionsPairs xs
    Buttons Horizontal -> radioField False $ optionsPairs xs) fs
  MultiChoice k fs xs -> renderForm (req $ case k of
    Dropdown -> multiSelectField $ optionsPairs xs
    Buttons Vertical -> checkboxField True $ optionsPairs xs
    Buttons Horizontal -> checkboxField False $ optionsPairs xs) fs


renderLayout :: Maybe (TypeList a) -> FormLayout a -> Rendered [[Widget]]
renderLayout mDefault (Single x) = applyToWidget (singleton . singleton) $
  flip renderRequiredness x $ fmap (\(TCons t TEmpty) -> t) mDefault
renderLayout mDefault (Beside x y) = renderLayout a x `horizontally` renderLayout b y
  where (a,b) = splitMaybeDefaults mDefault
renderLayout mDefault (Above x y) = renderLayout a x `vertically` renderLayout b y
  where (a,b) = splitMaybeDefaults mDefault
renderLayout mDefault (List align fs) =
    foldr1 addParams [renderLayout d (Single f) | (d,f) <- zip defaults $ toList fs]
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


splitMaybeDefaults :: Split xs ys => Maybe (TypeList (xs ++ ys)) -> (Maybe (TypeList xs), Maybe (TypeList ys))
splitMaybeDefaults Nothing = (Nothing, Nothing)
splitMaybeDefaults (Just xs) = (Just left, Just right)
  where (left, right) = splitTypeList xs


formify
  :: Formify a
  => Maybe a -- ^ Optional default value for form.
  -> FormSpec a -- ^ Structure of form.
  -> Rendered Widget -- ^ Rendered form.
formify mDefault = applyToWidget joinWidgets . formifyImplementation mDefault


zipWithEnum :: forall a. (Bounded a, Enum a) => [SomeMessage FlexForm] -> [(SomeMessage FlexForm, a)]
zipWithEnum labels
  | equalLength labels options = zip labels options
  | otherwise = error "Labels list and options list are of different lengths in an Enum choice form."
  where options = [minBound .. maxBound :: a]

dropdown
  :: FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [SomeMessage FlexForm]  -- ^ Option labels
  -> FieldInfo SingleChoiceSelection
dropdown fs opts = SingleChoice Dropdown fs $ zip opts $ map singleChoiceAnswer [1..]


dropdownMulti
  :: FieldSettings FlexForm  -- ^ FieldSettings for select input
  -> [SomeMessage FlexForm]  -- ^ Option labels
  -> FieldInfo MultipleChoiceSelection
dropdownMulti fs opts = MultiChoice Dropdown fs $ zip opts $ map singleChoiceAnswer [1..]


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
      let
        (leftInit, leftLast) = fromMaybe (xss,[]) $ unsnoc xss
        (rightHead, rightTail) = fromMaybe ([],yss) $ uncons yss
      pure (ids1 ++ ids2, nubOrd $ names1 ++ names2, leftInit ++ [leftLast ++ rightHead] ++ rightTail)



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


data Roflmao = Lol Int String Double deriving Generic

instance Formify Roflmao

singleReq :: FieldInfo a -> FormLayout '[a]
singleReq = Single . Required

singleOpt :: FieldInfo a -> FormLayout (FormType (Maybe a))
singleOpt = Single . Optional

test :: FormLayout ys -> FormLayout (Maybe Integer ':  ys)
test = Beside (Single $ Optional $ Basic "a")


formSpec :: FormLayout (FormType a) -> FormSpec a
formSpec = FormSpec



test2 :: FormSpec Integer
test2 = formSpec $ singleReq $ Basic "e"

testDerived :: Rendered Widget
testDerived = formify (Just (1,2)) $ formSpec @(Int,Int) $
  Single (Required $ Basic "Number") `Above` Single (Required $ Basic "Text")
