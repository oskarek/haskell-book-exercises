module Integers where

import           Control.Applicative
import           Data.Char           (digitToInt)
import           Data.List           (foldl')
import           Text.Trifecta

digit' :: Parser Char
digit' = satisfyRange '0' '9'

zeroNum :: Parser Integer
zeroNum =
  char '0' *> (nat <|> pure 0)

base10Integer :: Parser Integer
base10Integer =
  zeroNum <|> nat

nat :: Parser Integer
nat =
  foldl' (\x d -> 10*x + toInteger (digitToInt d)) 0
  <$> some digit'

sign :: Parser (Integer -> Integer)
sign =
  id <$ symbol "+"
  <|> negate <$ symbol "-"

base10Integer' :: Parser Integer
base10Integer' = option id sign <*> base10Integer
