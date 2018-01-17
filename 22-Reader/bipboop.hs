module BipBoop where

import           Control.Applicative

boop, doop :: Num a => a -> a
boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) c1 c2 = (||) <$> c1 <*> c2
