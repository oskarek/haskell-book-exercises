module MaybeAnotherMonoid where

import Data.Monoid
import Test.QuickCheck

data Optional a = Some a | None deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return None),
                         (3, fmap Some arbitrary)]

newtype First' a =
  First' { getFirst :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' None
  mappend = firstMappend

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = fmap First' arbitrary

firstMappend :: First' a -> First' a -> First' a
First' None `firstMappend` m = m
a           `firstMappend` _ = a

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

monoidAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
monoidAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = a <> mempty == a

(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (x:xs) l = x : (+++) xs l

class Semigroup a where
  (<|>) :: a -> a -> a

data NonEmpty a = a :| [a] deriving (Eq, Show)

instance Semigroup (NonEmpty a) where
  (<|>) (a :| as) (b :| bs) = a :| (as ++ b : bs)
