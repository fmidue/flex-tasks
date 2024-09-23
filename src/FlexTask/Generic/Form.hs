{-# language DefaultSignatures #-}
{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module FlexTask.Generic.Form
  ( Alignment(..)
  , FieldInfo
  , SingleChoiceSelection
  , MultipleChoiceSelection

  , BaseForm(..)
  , Formify(..)

  , formifyInstanceBase
  , formifyInstanceMultiChoice
  , formifyInstanceSingleChoice
  , formify

  , getAnswer
  , getAnswers
  , multipleChoiceAnswer
  , multipleChoiceEmpty
  , singleChoiceAnswer
  , singleChoiceEmpty

  , buttons
  , buttonsEnum
  , dropdown
  , dropdownEnum
  , list
  , listWithoutLabels
  , single
  ) where


import Data.Either         (isRight)
import Data.List.Extra     (nubSort)
import GHC.Generics        (Generic(..), K1(..), M1(..), (:*:)(..))
import GHC.Utils.Misc      (equalLength)
import Data.Text           (Text, pack, unpack)
import Yesod

import FlexTask.FormUtil   (($$>))
import FlexTask.Widgets
  ( horizontalRadioField
  , renderFlatOrBreak
  , verticalCheckboxesField
  )
import FlexTask.YesodConfig (FlexForm, Handler, Rendered)




data FieldInfo
  = Single Text
  | List Alignment [Text]
  | ChoicesDropdown Text [Text]
  | ChoicesButtons Alignment Text [Text]
  | InternalListElem Text
  deriving (Eq,Show)


data Alignment = Horizontal | Vertical deriving (Eq,Show)


newtype SingleChoiceSelection = SingleChoiceSelection {getAnswer :: Maybe Int} deriving (Show,Eq,Generic)
newtype MultipleChoiceSelection = MultipleChoiceSelection { getAnswers :: [Int]} deriving (Show,Eq,Generic)



singleChoiceEmpty :: SingleChoiceSelection
singleChoiceEmpty = SingleChoiceSelection Nothing


singleChoiceAnswer :: Int -> SingleChoiceSelection
singleChoiceAnswer = SingleChoiceSelection . Just


multipleChoiceEmpty :: MultipleChoiceSelection
multipleChoiceEmpty = MultipleChoiceSelection []

multipleChoiceAnswer :: [Int] -> MultipleChoiceSelection
multipleChoiceAnswer = MultipleChoiceSelection . nubSort



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



class Formify a where
  formifyImplementation
      :: Maybe a
      -> [[FieldInfo]]
      -> ([[FieldInfo]], Rendered)

  default formifyImplementation
      :: (Generic a, GFormify (Rep a))
      => Maybe a
      -> [[FieldInfo]]
      -> ([[FieldInfo]], Rendered)
  formifyImplementation mDefault = gformify $ from <$> mDefault



class GFormify f where
  gformify :: Maybe (f a) -> [[FieldInfo]] -> ([[FieldInfo]], Rendered)


-- | Products: parse a constructor with multiple arguments
instance (GFormify a, GFormify b) => GFormify (a :*: b) where
  gformify mDefault xs = (rightRest, leftRender $$> rightRender)
    where
      (left,right) = case mDefault of
        Nothing        -> (Nothing,Nothing)
        Just (a :*: b) -> (Just a, Just b)
      (leftRest, leftRender) = gformify left xs
      (rightRest, rightRender) = gformify right leftRest



-- | Meta-information (constructor names, etc.)
instance GFormify a => GFormify (M1 i c a) where
  gformify mDefault = gformify $ unM1 <$> mDefault


-- | Constants, additional parameters and recursion of kind *
instance Formify a => GFormify (K1 i a) where
  gformify mDefault = formifyImplementation $ unK1 <$> mDefault


instance Formify Int where
  formifyImplementation = formifyInstanceBase . Right


instance Formify Text where
  formifyImplementation = formifyInstanceBase . Right


instance Formify String where
  formifyImplementation = formifyInstanceBase . Right


instance Formify Bool where
  formifyImplementation = formifyInstanceBase . Right



instance Formify Double where
  formifyImplementation = formifyInstanceBase . Right


instance (Formify a, Formify b) => Formify (a,b)

instance (Formify a, Formify b, Formify c) => Formify (a,b,c)

instance (Formify a, Formify b, Formify c, Formify d) => Formify (a,b,c,d)


instance {-# Overlappable #-} (BaseForm a, Formify a) => Formify [a] where
  formifyImplementation = formifyInstanceList



instance (BaseForm a, Formify a) => Formify (Maybe a) where
  formifyImplementation = formifyInstanceBase . Left


instance Formify (Maybe a) => Formify [Maybe a] where
  formifyImplementation = formifyInstanceList


instance Formify SingleChoiceSelection where
  formifyImplementation = formifyInstanceChoice (`zip` [1..]) . Right . (=<<) getAnswer


instance Formify MultipleChoiceSelection where
  formifyImplementation = formifyInstanceChoice (`zip` [1..]) . Left . fmap getAnswers



formify :: Formify a => Maybe a -> [[FieldInfo]] -> Rendered
formify ma xs
      | null names = form
      | otherwise  = error "mismatched amount of field names and actual fields!"
    where
      (names, form) = formifyImplementation ma xs



formifyInstanceBase
    :: BaseForm a
    => Either (Maybe (Maybe a)) (Maybe a)
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceBase _ [] = error "Incorrect amount of field names"
formifyInstanceBase eMa ((x : xs) : xss) =
    (rest, case eMa of
        Right ma -> render newId (flip (areq baseForm) ma) lab
        Left mMa -> render newId (flip (aopt baseForm) mMa) lab
    )
  where
    (rest,render) = restAndRenderMethod xs xss
    (lab,newId) =
        case x of
          Single t -> (t,True)
          InternalListElem t -> (t,False)
          _ -> error "Internal mismatch of FieldInfo and rendering function"

formifyInstanceBase eMa _ = error $ "Incorrect naming scheme for "
    ++ if isRight eMa then "a simple field!" else "an optional value!"



formifyInstanceList
    :: Formify a
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



formifyInstanceSingleChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe a
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceSingleChoice = formifyInstanceChoice zipWithEnum . Right

formifyInstanceChoice
    :: Eq a
    => ([Text] -> [(Text, a)])
    -> Either (Maybe [a]) (Maybe a)
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceChoice _ _ [] = error "ran out of field names!"
formifyInstanceChoice pairsWith eMa ((ChoicesDropdown t opts : xs) : xss) =
    ( rest
    , case eMa of
        Right ma -> render True (
          flip (areq $ selectField $ optionsPairs (pairsWith opts)) ma) t
        Left mas -> render True (
          flip (areq $ multiSelectField $ optionsPairs (pairsWith opts)) mas) t
    )
  where
    (rest,render) = restAndRenderMethod xs xss

formifyInstanceChoice pairsWith eMa ((ChoicesButtons align t opts : xs) : xss) =
    ( rest
    , case eMa of
        Right ma  -> render True (flip (areq
            (
              case align of
                Vertical   -> radioField
                Horizontal -> horizontalRadioField
              $ optionsPairs (pairsWith opts)
            )
            )
            ma
            ) t

        Left mas -> render True (flip (areq
            (
              case align of
                Vertical   -> verticalCheckboxesField
                Horizontal -> checkboxesField
              $ optionsPairs (pairsWith opts)
            )) mas) t
    )
  where
    (rest,render) = restAndRenderMethod xs xss


formifyInstanceChoice _ eMa _ = error $ "Incorrect naming scheme for a "
    ++ if isRight eMa then "single choice!" else "multi choice!"

formifyInstanceMultiChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceMultiChoice = formifyInstanceChoice zipWithEnum . Left



zipWithEnum :: forall a. (Bounded a, Enum a) => [Text] -> [(Text, a)]
zipWithEnum labels
  | equalLength labels options = zip labels options
  | otherwise = error "Labels list and options list are of different lengths in an Enum choice form."
  where options = [minBound .. maxBound :: a]



restAndRenderMethod
    :: [FieldInfo]
    -> [[FieldInfo]]
    -> ( [[FieldInfo]]
       , Bool
          -> (FieldSettings FlexForm -> AForm Handler a)
          -> Text
          -> Rendered
       )
restAndRenderMethod [] xss = (xss,renderFlatOrBreak True)
restAndRenderMethod xs xss = (xs:xss, renderFlatOrBreak False)



buttonsEnum
  :: (Bounded a, Enum a)
  => Alignment
  -> Text
  -> (a -> Text)
  -> FieldInfo
buttonsEnum align t f = ChoicesButtons align t $ map f [minBound .. maxBound]



buttons :: Alignment -> Text -> [Text] -> FieldInfo
buttons = ChoicesButtons



dropdownEnum :: (Bounded a, Enum a) => Text -> (a -> Text) -> FieldInfo
dropdownEnum t f = ChoicesDropdown t $ map f [minBound .. maxBound]



dropdown :: Text -> [Text] -> FieldInfo
dropdown = ChoicesDropdown



list :: Alignment -> [Text] -> FieldInfo
list = List



listWithoutLabels :: Alignment -> Int -> FieldInfo
listWithoutLabels align amount = List align $ replicate amount ""



single :: Text -> FieldInfo
single = Single
