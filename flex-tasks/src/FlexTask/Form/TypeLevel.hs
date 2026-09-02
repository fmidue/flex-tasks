{-# language DataKinds #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module FlexTask.Form.TypeLevel where


import Data.Kind                        (Constraint, Type)
import Data.Tuple.Extra                 (first)
import GHC.TypeLits                     (ErrorMessage(..), TypeError)

import FlexTask.InputTypes              (MultipleChoice)


-- | A marker for type forms with exactly one input field
data OneField a


-- | A marker for type forms with multiple input fields, e.g. lists.
data ManyFields a


data TypeList xs where
  TOne :: OneDefault x -> TypeList (OneField x)
  TMany :: OneDefault x -> Maybe [OneDefault x] -> TypeList (ManyFields x)
  TCons :: InputDefault x -> TypeList xs -> TypeList (x :> xs)


-- | type-level non-empty list equivalent of "append" (++)
infixr 5 ++
type family xs ++ ys where
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


appendShape :: TypeShape xs -> TypeShape ys -> TypeShape (xs ++ ys)
appendShape OneShape ys = ConsShape ys
appendShape ManyShape ys = ConsShape ys
appendShape (ConsShape xs) ys = ConsShape (appendShape xs ys)


-- | Type-level non-empty list equivalent of "cons" (:)
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


-- | A constraint for types that can meaningfully be made optional.
type family CanBeOptional a :: Constraint where
  CanBeOptional (MultipleChoice a) =
    TypeError
      ( 'Text "MultipleChoice cannot be optional."
        ':$$:
        'Text "No selection is represented by the empty list."
      )
  CanBeOptional [a] =
    TypeError
      ( 'Text "Lists cannot be optional as a whole."
        ':$$:
        'Text "Use type [Maybe a] to turn all fields optional instead."
      )
  CanBeOptional a = ()
