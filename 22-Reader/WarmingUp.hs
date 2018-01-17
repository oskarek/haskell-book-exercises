module WarmingUp where

import           Control.Applicative
import           Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- cap
  b <- rev
  return (a,b)

tupled2 :: [Char] -> ([Char], [Char])
tupled2 = liftA2 (,) cap rev

tupled3 :: [Char] -> ([Char], [Char])
tupled3 = cap >>=
            \a -> rev >>=
              \b -> return (a,b)

-- Dog <$> dogName
-- (DogName -> Address -> Dog) <$> (Person -> DogName)
-- (Person -> Address -> Dog)
--
--
-- (<*>) :: Applicative t => t (a -> b) -> t a -> t b
-- spec :: (r -> a -> b) (r -> a) -> r -> b
--
-- (Person -> Address -> Dog) <*> (Person -> Address)
-- (Person -> Dog)
--
-- -- (.) ::  (b -> c) -> (a -> b) -> (a -> c)
-- -- fmap :: Functor f => (a -> b) -> f a -> f b
--
