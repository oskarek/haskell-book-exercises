module MonoidExercises where

import Data.Monoid
import Test.QuickCheck (Arbitrary, arbitrary,
                        CoArbitrary, coarbitrary,
                        frequency)

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = a <> mempty == a

-- 1. Trivial --
data Trivial = Trivial deriving (Eq, Show)

instance Monoid Trivial where
  mempty = Trivial
  _ `mappend` _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- 2. Identity --
newtype Identity a = Identity a deriving (Eq, Show)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  (Identity a) `mappend` (Identity b) = Identity (a `mappend` b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

-- 3. Two --
data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  Two a b `mappend` Two a' b' =
    Two (a `mappend` a') (b `mappend` b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- 4. BoolConj --
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Monoid BoolConj where
  mempty = BoolConj True
  BoolConj a `mappend` BoolConj b = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj arbitrary

-- 5. BoolDisj --
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  BoolDisj a `mappend` BoolDisj b = BoolDisj (a && b)

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj arbitrary

-- 6. Combine --
newtype Combine a b =
  Combine { unCombine :: a -> b }

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  Combine f `mappend` Combine f' =
    Combine (\a -> f a `mappend` f' a)

-- 7. Comp --
newtype Comp a =
  Comp (a -> a)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp id
  Comp a `mappend` Comp a' = Comp (a' . a)

-- 8. Men --
newtype Men s a =
  Men {
    runMen :: s -> (a,s)
  }

instance Monoid a => Monoid (Men s a) where
  mempty = Men (\s -> (mempty, s))
  Men f `mappend` Men f' =
    let comb (a,s) = let (a',s') = f' s
                     in (a <> a', s')
    in Men (comb . f)
