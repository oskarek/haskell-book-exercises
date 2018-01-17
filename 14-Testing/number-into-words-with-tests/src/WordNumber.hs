module WordNumber where

import Data.List (intercalate)

-- exercises from Recursion chapter --
digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "unknown"

digits :: Int -> [Int]
digits n = case n `divMod` 10 of
  (0,x) -> [n]
  (div', mod') -> digits div' ++ [mod']

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
