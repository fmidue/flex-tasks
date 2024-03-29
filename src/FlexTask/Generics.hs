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
      :: [[FieldInfo]]
      -> Maybe a
      -> ([[FieldInfo]], Rendered)
  parseInput :: Parser a

  default formifyImplementation
      :: (Generic a, GFormify (Rep a))
      => [[FieldInfo]]
      -> Maybe a
      -> ([[FieldInfo]], Rendered)
  formifyImplementation xs mDefault = gformify xs $ from <$> mDefault

  default parseInput :: (Generic a, GFormify (Rep a)) => Parser a
  parseInput = to <$> gparse



class GFormify f where
  gformify :: [[FieldInfo]] -> Maybe (f a) -> ([[FieldInfo]], Rendered)

  gparse :: Parser (f a)

-- | Products: parse a constructor with multiple arguments
instance (GFormify a, GFormify b) => GFormify (a :*: b) where
  gformify xs mDefault = (rightRest, gRender)
    where
      (left,right) = case mDefault of
        Nothing        -> (Nothing,Nothing)
        Just (a :*: b) -> (Just a, Just b)
      (leftRest, leftRender) = gformify xs left
      (rightRest, rightRender) = gformify leftRest right

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
  gformify xs mDefault = gformify xs (unM1 <$> mDefault)
  gparse = M1 <$> gparse


-- | Constants, additional parameters and recursion of kind *
instance Formify a => GFormify (K1 i a) where
  gformify xs mDefault = formifyImplementation xs $ unK1 <$> mDefault
  gparse = K1 <$> parseInput


instance Formify Int where
  formifyImplementation = formifyInstanceBase
  parseInput = intParse


instance Formify Text where
  formifyImplementation = formifyInstanceBase
  parseInput = escaped $ do
    input <- manyTill anyChar $ try $ lookAhead $
      string escape >> notFollowedBy (string "\"")
    pure $ T.pack $ read ('\"' : input ++ "\"")


instance Formify Bool where
  formifyImplementation = formifyInstanceBase
  parseInput = escaped $ do
    val <- try (string "yes") <|> string "no"
    pure $ case val of
            "yes" -> True
            _     -> False


instance Formify Double where
  formifyImplementation = formifyInstanceBase
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
  parseInput = try (escaped parseEmpty) <|> sepBy parseInput (string listDelim)
    where
      parseEmpty = string "Missing" >> pure []


instance (BaseForm a, Formify a) => Formify (Maybe a) where
  formifyImplementation [] _ = error "ran out of field names"
  formifyImplementation ((Single x : xs) : xss) ma =
      (rest, render $ aopt baseForm (addName x) ma)
    where
      (rest,render) = restAndRenderMethod xs xss
  formifyImplementation _ _ = error "Incorrect naming scheme for an optional value!"

  parseInput = do
    mMissing <- optionMaybe $ try $ escaped $ string "None"
    case mMissing of
      Nothing -> Just <$> parseInput
      Just _  -> pure Nothing


instance Formify (Maybe a) => Formify [Maybe a] where
  formifyImplementation = formifyInstanceList
  parseInput = try (escaped parseEmpty) <|> sepBy parseInput (string listDelim)
    where
      parseEmpty = string "Missing" >> pure []



addName :: Text -> FieldSettings FlexForm
addName name = (fieldSettingsLabel name) {fsId = Just name, fsName = Just name}



formify :: Formify a => [[FieldInfo]] -> Maybe a -> Rendered
formify xs ma
      | null names = form
      | otherwise  = error "mismatched amount of field names and actual fields! "
    where
      (names, form) = formifyImplementation xs ma



formifyInstanceBase
    :: BaseForm a
    => [[FieldInfo]]
    -> Maybe a
    -> ([[FieldInfo]], Rendered)
formifyInstanceBase [] _ = error "Incorrect amount of field names"
formifyInstanceBase ((Single t : xs) : xss) ma = (rest, render $ areq baseForm (addName t) ma)
  where (rest,render) = restAndRenderMethod xs xss

formifyInstanceBase _ _ = error "Incorrect naming scheme for a simple field!"



formifyInstanceList
    :: Formify a
    => [[FieldInfo]]
    -> Maybe [a]
    -> ([[FieldInfo]], Rendered)
formifyInstanceList [] _ = error "ran out of field names"
formifyInstanceList ((List _ [] : _) : _) _ = error "List of fields without names!"
formifyInstanceList ((List align fs : xs) : xss) mas =
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
      let (remain,render) = formifyImplementation m (head defs)
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
    => [[FieldInfo]]
    -> Maybe a
    -> ([[FieldInfo]], Rendered)
formifyInstanceSingleChoice [] _ = error "ran out of field names!"
formifyInstanceSingleChoice ((ChoicesDropdown t opts : xs) : xss) ma =
    (rest, render $ areq (selectField $ enumOptionsPairs opts) (addName t) ma)
  where
    (rest,render) = restAndRenderMethod xs xss

formifyInstanceSingleChoice ((ChoicesButtons align t opts : xs) : xss) ma = (rest, form)
  where
    (rest,render) = restAndRenderMethod xs xss
    form = render $ areq (field $ enumOptionsPairs opts) (addName t) ma

    field = case align of
      Vertical   -> radioField
      Horizontal -> horizontalRadioField

formifyInstanceSingleChoice _ _ = error "Incorrect naming scheme for a single choice!"



formifyInstanceMultiChoice
    :: (Bounded a, Enum a, Eq a)
    => [[FieldInfo]]
    -> Maybe [a]
    -> ([[FieldInfo]], Rendered)
formifyInstanceMultiChoice [] _ = error "ran out of field names"
formifyInstanceMultiChoice ((ChoicesDropdown t opts : xs) : xss) ma =
    (rest, render $ areq (multiSelectField $ enumOptionsPairs opts) (addName t) ma)
  where
    (rest,render) = restAndRenderMethod xs xss

formifyInstanceMultiChoice ((ChoicesButtons align t opts : xs) : xss) ma =
    (rest, render $ areq toMultiForm (addName t) ma)
  where
    (rest,render) = restAndRenderMethod xs xss

    field = case align of
      Vertical   -> verticalCheckboxesField
      Horizontal -> checkboxesField

    toMultiForm = field $ enumOptionsPairs opts

formifyInstanceMultiChoice _ _ = error "Incorrect naming scheme for a multi choice!"



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

