module TryTry where

import           Control.Applicative
import           Fractions
import           Text.Trifecta

type RationalOrInteger = Either Rational Integer

doubleParser :: Parser Double
doubleParser = double

decimalParser :: Parser Integer
decimalParser = decimal

rationalOrIntegerParser :: Parser RationalOrInteger
rationalOrIntegerParser =
  try (Left <$> parseFraction) <|>
  (Right <$> decimalParser)
