{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module FlexTask.Form.Formify where


import Control.Monad                    (join)
import Data.Kind                        (Constraint)
import Data.Text                        (Text)
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
import Yesod                            (Textarea)

import FlexTask.Form.TypeLevel (
  type (++),
  CanBeOptional,
  ManyFields,
  OneDefault(..),
  OneField,
  SingleInputType,
  TypeList(..),
  appendTypeList,
  getSingleDefault,
  singleFormDefaults,
  )
import FlexTask.InputTypes (
  Hidden,
  MultipleChoice,
  SingleChoiceSelection,
  SingleInputList,
  )


{- |
Class for generic derivation of overall form types.
Any type you want to create a completed form for needs to be an instance of this type.
Bodyless instances can be declared for most types deriving Generic.
Alternatively, you can also derive Formify itself using `DeriveAnyClass`.

__Generic derivation is not supported for:__

  * types with a single nullary constructor
  * sum types in which any constructor has fields

__Manually implementing Formify's internals is not supported.__
-}
class Formify a where

  {- |
  The type-level non-empty list of types needed for a complete form, e.g.

  @OneField Int@ for Int

  @ManyFields Text@ for [Text]

  @OneField Text :> OneField Bool@ for (Text,Bool)
  -}
  type FormTypes a

  type FormTypes a = GFormTypes a (Rep a)

  formDefaults :: Maybe a -> TypeList (FormTypes a)

  default formDefaults
    :: (Generic a, GFormDefaults a (Rep a), FormTypes a ~ GFormTypes a (Rep a))
    => Maybe a
    -> TypeList (FormTypes a)
  formDefaults = gFormDefaults @a . fmap from


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


instance {-# Overlappable #-} Formify a => Formify [a] where
  type FormTypes [a] = ManyFields (SingleInputType (FormTypes a))
  formDefaults mValues = TMany t a
    where
      t = getSingleDefault $ formDefaults @a Nothing
      a = map (getSingleDefault . formDefaults . Just) <$> mValues


instance CanBeOptional a => Formify (Maybe a) where
  type FormTypes (Maybe a) = OneField a
  formDefaults m = TOne $ OptionalDefault $ join m


instance Formify SingleChoiceSelection where
  type FormTypes SingleChoiceSelection = OneField SingleChoiceSelection
  formDefaults = singleFormDefaults


instance Formify (MultipleChoice a) where
  type FormTypes (MultipleChoice a) = OneField (MultipleChoice a)
  formDefaults = singleFormDefaults


instance (Formify a, Formify b) => Formify (a,b)
instance (Formify a, Formify b, Formify c) => Formify (a,b,c)
instance (Formify a, Formify b, Formify c, Formify d) => Formify (a,b,c,d)
instance (Formify a, Formify b, Formify c, Formify d, Formify e) => Formify (a,b,c,d,e)
instance (Formify a, Formify b, Formify c, Formify d, Formify e, Formify f) => Formify (a,b,c,d,e,f)


type family GFormTypes original rep where
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
      ( 'Text "Cannot derive Formify for this type." ':$$:
        'Text "A sum type must contain only nullary constructors," ':$$:
        'Text "but at least one constructor contains fields." ':$$:
        'Text "This is not supported."
      )
