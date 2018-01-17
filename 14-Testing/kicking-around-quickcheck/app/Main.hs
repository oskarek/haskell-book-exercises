module Main where

import Arbitrary
import CoArbitrary
import Test.QuickCheck

main :: IO ()
main =
  sample sumGenCharIntFirst
