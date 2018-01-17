module Scans where

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

factorials :: [Integer]
factorials = scanl (*) 1 [2..]
