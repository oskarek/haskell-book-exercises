module TransformingLists where

import Data.Char

firstCapitalized :: String -> Char
firstCapitalized = toUpper . head
