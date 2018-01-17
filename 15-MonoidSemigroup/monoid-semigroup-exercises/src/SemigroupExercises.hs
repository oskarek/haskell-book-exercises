module SemigroupExercises where

import Test.QuickCheck (Arbitrary, arbitrary,
                        CoArbitrary, coarbitrary,
                        frequency)
import Data.Semigroup

semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

-- 1. trivial --
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- 2. Identity --
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

-- 3. Two --
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- 4. Three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
          => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' =
    Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c)
          => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

-- 5. Four --
-- ORKA --

-- 6. BoolConj --
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj a <> BoolConj b = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj arbitrary

-- 7. BoolDist --
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj arbitrary

-- 8. Or --
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  Fst _ <> o = o
  o     <> _ = o

-- 9. Combine --
newtype Combine a b =
  Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine f' = Combine (\a -> f a <> f' a)


-- 11. Validation --
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (Validation a b) where
    Success s <> Success s' = Success (s <> s')
    Failure f <> _          = Failure f
    _         <> Failure f  = Failure f

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Validation a b) where
    arbitrary = frequency [(3, fmap Success arbitrary),
                           (1, fmap Failure arbitrary)]
