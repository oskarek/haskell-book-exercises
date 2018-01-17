module Main where

import Criterion.Main

myList :: [Int]
myList = [1..9999]

main :: IO ()
main = defaultMain
  [ bench "map list 9999" $
    nf (map (+)) myList -- whnf doesn't work here because of guarded recursion
  ]
