{-# language DefaultSignatures #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module FlexTask.Generics
  ( Alignment(..)
  , FieldInfo(..)

  , BaseForm(..)
  , Formify(..)

  , formifyInstanceMultiChoice
  , formifyInstanceSingleChoice
  , parseInstanceSingleChoice
  , parseInstanceMultiChoice

  , addName
  , formify

  , escape
  , argDelim
  , listDelim
  , escaped
  , parseUnicode
  ) where


import Control.Monad       (void)
import Data.Char           (showLitChar)
import Data.Either         (isRight)
import GHC.Generics        (Generic(..), K1(..), M1(..), (:*:)(..))
import Data.Text           (Text)
import Text.Parsec
  ( (<|>)
  , between
  , lookAhead
  , manyTill
  , many1
  , notFollowedBy
  , optionMaybe
  , sepBy
  , try
  )
import Text.Parsec.Char    (anyChar, char, digit, string)
import Text.Parsec.String  (Parser)
import Yesod

import qualified Data.Text as T

import FlexTask.Widgets
  ( horizontalRadioField
  , renderFlatOrBreak
  , verticalCheckboxesField
  )
import FlexTask.YesodConfig (FlexForm, Handler, Widget)




data FieldInfo
  = Single Text
  | List Alignment [Text]
  | ChoicesDropdown Text [Text]
  | ChoicesButtons Alignment Text [Text]
  deriving (Eq,Show)


data Alignment = Horizontal | Vertical deriving (Eq,Show)


type Rendered = Html -> MForm Handler Widget



class BaseForm a where
  baseForm :: Field Handler a


instance BaseForm Int where
  baseForm = intField


instance BaseForm Text where
  baseForm = textField


instance BaseForm Bool where
  baseForm = boolField


instance BaseForm Double where
  baseForm = doubleField



class Formify a where
  formifyImplementation
      :: Maybe a
      -> [[FieldInfo]]
      -> ([[FieldInfo]], Rendered)
  parseInput :: Parser a

  default formifyImplementation
      :: (Generic a, GFormify (Rep a))
      => Maybe a
      -> [[FieldInfo]]
      -> ([[FieldInfo]], Rendered)
  formifyImplementation mDefault = gformify $ from <$> mDefault

  default parseInput :: (Generic a, GFormify (Rep a)) => Parser a
  parseInput = to <$> gparse



class GFormify f where
  gformify :: Maybe (f a) -> [[FieldInfo]] -> ([[FieldInfo]], Rendered)

  gparse :: Parser (f a)

-- | Products: parse a constructor with multiple arguments
instance (GFormify a, GFormify b) => GFormify (a :*: b) where
  gformify mDefault xs = (rightRest, gRender)
    where
      (left,right) = case mDefault of
        Nothing        -> (Nothing,Nothing)
        Just (a :*: b) -> (Just a, Just b)
      (leftRest, leftRender) = gformify left xs
      (rightRest, rightRender) = gformify right leftRest

      gRender = do
        wid1 <- leftRender
        wid2 <- rightRender
        pure $ (>>) <$> wid1 <*> wid2

  gparse = do
    a <- gparse
    void $ string argDelim
    b <- gparse
    pure (a :*: b)



-- | Meta-information (constructor names, etc.)
instance GFormify a => GFormify (M1 i c a) where
  gformify mDefault = gformify $ unM1 <$> mDefault
  gparse = M1 <$> gparse


-- | Constants, additional parameters and recursion of kind *
instance Formify a => GFormify (K1 i a) where
  gformify mDefault = formifyImplementation $ unK1 <$> mDefault
  gparse = K1 <$> parseInput


instance Formify Int where
  formifyImplementation = formifyInstanceBase . Right
  parseInput = intParse


instance Formify Text where
  formifyImplementation = formifyInstanceBase . Right
  parseInput = escaped $ do
    input <- manyTill anyChar $ try $ lookAhead $
      string escape >> notFollowedBy (string "\"")
    pure $ T.pack $ read ('\"' : input ++ "\"")


instance Formify Bool where
  formifyImplementation = formifyInstanceBase . Right
  parseInput = escaped $ do
    val <- try (string "yes") <|> string "no"
    pure $ case val of
            "yes" -> True
            _     -> False


instance Formify Double where
  formifyImplementation = formifyInstanceBase . Right
  parseInput = escaped $ do
    sign <- optionMaybe $ char '-'
    whole <- many1 digit
    dot <- optionMaybe (char '.' <|> char ',')
    frac <- case dot of
              Nothing -> pure []
              Just _  -> ('.':) <$> many1 digit
    pure $ read $ case sign of
                    Nothing ->     whole
                    Just s  -> s : whole
                  ++ frac



instance (Formify a, Formify b) => Formify (a,b)

instance (Formify a, Formify b, Formify c) => Formify (a,b,c)

instance (Formify a, Formify b, Formify c, Formify d) => Formify (a,b,c,d)


instance {-# Overlappable #-} (BaseForm a, Formify a) => Formify [a] where
  formifyImplementation = formifyInstanceList
  parseInput = parseInstanceList

parseInstanceList :: Formify a => Parser [a]
parseInstanceList =
  try (escaped parseEmpty) <|> sepBy parseInput (string listDelim)
    where
      parseEmpty = string "Missing" >> pure []


instance (BaseForm a, Formify a) => Formify (Maybe a) where
  formifyImplementation = formifyInstanceBase . Left

  parseInput = do
    mMissing <- optionMaybe $ try $ escaped $ string "None"
    case mMissing of
      Nothing -> Just <$> parseInput
      Just _  -> pure Nothing


instance Formify (Maybe a) => Formify [Maybe a] where
  formifyImplementation = formifyInstanceList
  parseInput = parseInstanceList



addName :: Text -> FieldSettings FlexForm
addName name = (fieldSettingsLabel name) {fsId = Just name, fsName = Just name}



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
formifyInstanceBase eMa ((Single t : xs) : xss) =
    (rest, case eMa of
        Right ma -> render $ areq baseForm (addName t) ma
        Left ma -> render $ aopt baseForm (addName t) ma
    )
  where (rest,render) = restAndRenderMethod xs xss
formifyInstanceBase eMa _ = error $ "Incorrect naming scheme for "
    ++ if isRight eMa then "a simple field!" else "an optional value!"



formifyInstanceList
    :: Formify a
    => Maybe [a]
    -> [[FieldInfo]]
    -> ([[FieldInfo]], Rendered)
formifyInstanceList _ [] = error "ran out of field names"
formifyInstanceList _ ((List _ [] : _) : _) = error "List of fields without names!"
formifyInstanceList mas ((List align fs : xs) : xss) =
    (rest, snd <$> foldr joinThem zero theList)
  where
    lastInRow = null xs

    (rest,theList,stopCondition) = case align of
      Horizontal
        | lastInRow -> ( xss, [[[Single f | f <- fs]]], null)
        | otherwise -> ( xs:xss
                       , [[[Single f | f <- fs ++ [undefined]]]]
                       , any $ (<= 1) . length
                       )

      Vertical
        | lastInRow -> ( xss, [[[Single f]]| f <- fs], null)
        | otherwise -> ( xs:xss
                       , [[Single f : [undefined | last fs == f]] | f <- fs]
                       , \ds -> null ds || length (last ds) <= 1
                       )

    sequencedDefaults = case mas of
      Nothing -> replicate (length fs) Nothing
      Just ds -> if length ds /= length fs
                   then error "Not enough values in the default list!"
                   else sequence mas

    defOrder = case align of
      Horizontal -> sequencedDefaults
      Vertical   -> reverse sequencedDefaults

    zero = pure (defOrder, pure $ pure ())

    joinThem m mList = do
      (defs,inner) <- mList
      let (remain,render) = formifyImplementation (head defs) m
      new <- render
      old <- if stopCondition remain
               then pure inner
               else snd <$> joinThem remain (pure (tail defs, inner))
      pure ( tail defs
           , do
               w <- new
               accumW <- old
               pure (w >> accumW)
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
        Right ma -> render $
          areq (selectField $ enumOptionsPairs opts) (addName t) ma
        Left mas -> render $
          areq (multiSelectField $ enumOptionsPairs opts) (addName t) mas
    )
  where
    (rest,render) = restAndRenderMethod xs xss

formifyInstanceChoice eMa ((ChoicesButtons align t opts : xs) : xss) =
    ( rest
    , case eMa of
        Right ma  -> render $ areq
            (
              case align of
                Vertical   -> radioField
                Horizontal -> horizontalRadioField
              $ enumOptionsPairs opts
            )
            (addName t)
            ma
        Left mas -> render $ areq
            (
              case align of
                Vertical   -> verticalCheckboxesField
                Horizontal -> checkboxesField
              $ enumOptionsPairs opts
            )
            (addName t)
            mas
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



enumOptionsPairs :: (Bounded a, Enum a) => [Text] -> Handler (OptionList a)
enumOptionsPairs labels = optionsPairs $ zip labels [minBound..maxBound]



restAndRenderMethod
    :: [FieldInfo]
    -> [[FieldInfo]]
    -> ([[FieldInfo]],AForm Handler a -> Html -> MForm Handler Widget)
restAndRenderMethod [] xss = (xss,renderFlatOrBreak True)
restAndRenderMethod xs xss = (xs:xss, renderFlatOrBreak False)



intParse :: Parser Int
intParse = escaped $ do
        sign <- optionMaybe $ char '-'
        ds <- many1 digit
        pure $ read $ case sign of
            Nothing -> ds
            Just s  -> s : ds



parseInstanceSingleChoice :: (Bounded a, Enum a, Eq a) => Parser a
parseInstanceSingleChoice = toEnum . subtract 1 <$> parseInput



parseInstanceMultiChoice :: (Bounded a, Enum a, Eq a) => Parser [a]
parseInstanceMultiChoice = fmap (toEnum . subtract 1) <$> parseInput



escape :: String
escape = "\"\""



argDelim :: String
argDelim = "\a\a"



listDelim :: String
listDelim = "\b\b"



escaped :: Parser a -> Parser a
escaped = between escParse escParse
  where escParse = string escape



parseUnicode :: Char -> Parser String
parseUnicode c = string $ showLitChar c ""
