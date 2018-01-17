-- stack ghc -- -prof -fprof-auto -rtsopts -O2 loci.hs
-- ./loci +RTS -hc -p
module Main where

import Control.Monad

blah :: [Integer]
blah = [1..1000]

main :: IO ()
main =
  replicateM_ 10000 (print blah)
