{-# language DefaultSignatures #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}

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
  , buttonsAnonymous
  , buttonsAnonymousNoHeader
  , buttonsForEnum
  , buttonsForEnumNoHeader
  , dropdownAnonymous
  , dropdownAnonymousNoHeader
  , dropdownForEnum
  , dropdownForEnumNoHeader
  , list
  , listNoLabels
  , listSomeLabels
  , single
  , singleNoLabel
  ) where


import Data.Either         (isRight)
import Data.Maybe          (fromMaybe)
import GHC.Generics        (Generic(..), K1(..), M1(..), (:*:)(..))
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
  | InternalListElem Text -- Not exported
  deriving (Eq,Show)


data Alignment = Horizontal | Vertical deriving (Eq,Show)


newtype SelectionOption = SelectionOption Int deriving (Bounded, Enum, Show, Eq)
type SingleChoiceSelection = SelectionOption
type MultipleChoiceSelection = [SelectionOption]



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


instance Formify SelectionOption where
  formifyImplementation = formifyInstanceSingleChoice


instance Formify [SelectionOption] where
  formifyImplementation = formifyInstanceMultiChoice



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
formifyInstanceSingleChoice = formifyInstanceChoice . Right

formifyInstanceChoice
    :: (Bounded a, Enum a, Eq a)
    => Either (Maybe [a]) (Maybe a)
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceChoice _ [] = error "ran out of field names!"
formifyInstanceChoice eMa ((ChoicesDropdown t opts : xs) : xss) =
    ( rest
    , case eMa of
        Right ma -> render True (
          flip (areq $ selectField $ enumOptionsPairs opts) ma) t
        Left mas -> render True (
          flip (areq $ multiSelectField $ enumOptionsPairs opts) mas) t
    )
  where
    (rest,render) = restAndRenderMethod xs xss

formifyInstanceChoice eMa ((ChoicesButtons align t opts : xs) : xss) =
    ( rest
    , case eMa of
        Right ma  -> render True (flip (areq
            (
              case align of
                Vertical   -> radioField
                Horizontal -> horizontalRadioField
              $ enumOptionsPairs opts
            )
            )
            ma
            ) t

        Left mas -> render True (flip (areq
            (
              case align of
                Vertical   -> verticalCheckboxesField
                Horizontal -> checkboxesField
              $ enumOptionsPairs opts
            )) mas) t
    )
  where
    (rest,render) = restAndRenderMethod xs xss


formifyInstanceChoice eMa _ = error $ "Incorrect naming scheme for a "
    ++ if isRight eMa then "single choice!" else "multi choice!"

formifyInstanceMultiChoice
    :: (Bounded a, Enum a, Eq a)
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceMultiChoice = formifyInstanceChoice . Left



enumOptionsPairs :: forall a. (Bounded a, Enum a) => [Text] -> Handler (OptionList a)
enumOptionsPairs labels
  | length labels == length options = optionsPairs (zip labels options)
  | otherwise = error "IMPOSSIBLE!"
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



buttonsForEnum
  :: Alignment
  -> Text
  -> (forall a. (Bounded a, Enum a) => (a -> Maybe String) -> FieldInfo)
buttonsForEnum align t s =
    ChoicesButtons align t $ map (maybe "" pack . s) [minBound .. maxBound]



buttonsForEnumNoHeader
  :: Alignment
  -> (forall a. (Bounded a, Enum a) => (a -> Maybe String) -> FieldInfo)
buttonsForEnumNoHeader align = buttonsForEnum align ""



buttonsAnonymous
  :: Alignment
  -> Text
  -> Int
  -> (Int -> Maybe String)
  -> FieldInfo
buttonsAnonymous align t amount s =
    ChoicesButtons align t $ map (maybe "" pack . s) $ take amount [1 .. maxBound]


buttonsAnonymousNoHeader
  :: Alignment
  -> Int
  -> (Int -> Maybe String)
  -> FieldInfo
buttonsAnonymousNoHeader align = buttonsAnonymous align ""


dropdownForEnum
  :: Text
  -> (forall a. (Bounded a, Enum a) => (a -> String) -> FieldInfo)
dropdownForEnum t s =
    ChoicesDropdown t $ map (pack . s) [minBound .. maxBound]



dropdownForEnumNoHeader
  :: (forall a. (Bounded a, Enum a) => (a -> String) -> FieldInfo)
dropdownForEnumNoHeader = dropdownForEnum ""



dropdownAnonymous :: Text -> Int -> (Int -> String) -> FieldInfo
dropdownAnonymous t amount s =
    ChoicesDropdown t $ map (pack .s) $ take amount [1 .. maxBound]



dropdownAnonymousNoHeader :: Int -> (Int -> String) -> FieldInfo
dropdownAnonymousNoHeader = dropdownAnonymous ""



list :: Alignment -> [Text] -> FieldInfo
list = List


listNoLabels :: Alignment -> Int -> FieldInfo
listNoLabels align amount = List align $ replicate amount ""


listSomeLabels :: Alignment -> [Maybe Text] -> FieldInfo
listSomeLabels align = List align . map (fromMaybe "")


single :: Text -> FieldInfo
single = Single


singleNoLabel :: FieldInfo
singleNoLabel = Single ""
