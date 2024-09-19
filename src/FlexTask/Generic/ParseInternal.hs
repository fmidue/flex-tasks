{-# language DefaultSignatures #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}


module FlexTask.Generic.ParseInternal
  ( Parse(..)
  , parseInstanceSingleChoice
  , parseInstanceMultiChoice
  , escaped
  ) where


import Control.Monad      (void)
import Data.Text          (Text)
import GHC.Generics       (Generic(..), K1(..), M1(..), (:*:)(..))
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
import Text.Parsec.Char   (anyChar, char, digit, string)
import Text.Parsec.String (Parser)

import qualified Data.Text    as T

import FlexTask.Processing.Text (argDelimiter, listDelimiter, inputEscape)
import FlexTask.Generic.Form (SingleChoiceSelection, MultipleChoiceSelection)



class Parse a where
  parseInput :: Parser a

  default parseInput :: (Generic a, GParse (Rep a)) => Parser a
  parseInput = to <$> gparse



class GParse f where
  gparse :: Parser (f a)



instance (GParse a, GParse b) => GParse (a :*: b) where
  gparse = do
    a <- gparse
    void $ parseText argDelimiter
    b <- gparse
    pure (a :*: b)



-- | Meta-information (constructor names, etc.)
instance GParse a => GParse (M1 i c a) where
  gparse = M1 <$> gparse



-- | Constants, additional parameters and recursion of kind *
instance Parse a => GParse (K1 i a) where
  gparse = K1 <$> parseInput



instance Parse Int where
  parseInput = escaped $ do
    sign <- optionMaybe $ char '-'
    ds <- many1 digit
    pure $ read $ case sign of
      Nothing -> ds
      Just s  -> s : ds



instance Parse String where
  parseInput = escaped $ manyTill anyChar $ try $ lookAhead $
      escape >> notFollowedBy (string "\"")



instance Parse Text where
  parseInput = T.pack <$> parseInput



instance Parse Bool where
  parseInput = escaped $ do
    val <- try (string "yes") <|> string "no"
    pure $ case val of
            "yes" -> True
            _     -> False



instance Parse Double where
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



instance (Parse a, Parse b) => Parse (a,b)

instance (Parse a, Parse b, Parse c) => Parse (a,b,c)

instance (Parse a, Parse b, Parse c, Parse d) => Parse (a,b,c,d)



instance {-# Overlappable #-} Parse a => Parse [a] where
  parseInput = try (escaped parseEmpty) <|> sepBy parseInput (parseText listDelimiter)
    where
      parseEmpty = string "Missing" >> pure []



instance Parse a => Parse (Maybe a) where
  parseInput = do
    mMissing <- optionMaybe $ try $ escaped $ string "None"
    case mMissing of
      Nothing -> Just <$> parseInput
      Just _  -> pure Nothing


instance Parse SingleChoiceSelection where
  parseInput = parseInstanceSingleChoice


instance Parse MultipleChoiceSelection where
  parseInput = parseInstanceMultiChoice


parseInstanceSingleChoice :: (Bounded a, Enum a, Eq a) => Parser a
parseInstanceSingleChoice = toEnum . subtract 1 <$> parseInput



parseInstanceMultiChoice :: (Bounded a, Enum a, Eq a) => Parser [a]
parseInstanceMultiChoice = fmap (toEnum . subtract 1) <$> parseInput



escape :: Parser String
escape = esc >> esc
  where esc = parseText inputEscape



escaped :: Parser a -> Parser a
escaped = between escape escape



parseText :: Text -> Parser String
parseText t = string $ T.unpack t
