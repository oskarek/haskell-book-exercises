{-# LANGUAGE OverloadedStrings #-}

module Fractions where

import           Control.Applicative
import           Data.Attoparsec.Text (parseOnly)
import           Data.Ratio           ((%))
import           Data.String          (IsString)
import           Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

badFractionT = "1/0"
alsoBadT = "10"
shouldWorkT = "1/2"
shouldAlsoWorkT = "2/1"

nonZeroDecimal :: (Monad m, TokenParsing m)
               => m Integer
nonZeroDecimal = do
  n <- decimal
  case n of
    0 -> fail "unexpected zero"
    _ -> return n

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- nonZeroDecimal
  return (numerator % denominator)

parseFraction' :: (Monad m, TokenParsing m)
                => m Rational
parseFraction' =
  (%) <$> decimal <* char '/' <*> nonZeroDecimal

mainFraction :: IO ()
mainFraction = do
  -- parseOnly is Attoparsec
  let attoP = parseOnly parseFraction'
  print $ attoP badFractionT
  print $ attoP shouldWorkT
  print $ attoP shouldAlsoWorkT
  print $ attoP alsoBadT

  -- parseString
  let p =
        parseString parseFraction' mempty
  print $ p badFraction
  print $ p shouldWork
  print $ p shouldAlsoWork
  print $ p alsoBad
