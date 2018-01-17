module ApplicativeChapterExercises where

import Control.Applicative (liftA3)

-- Pair --
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a

  Pair f f' <*> Pair a a' = Pair (f a) (f' a')

-- Two --
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty

  Two a f <*> Two a' b = Two (a `mappend` a') (f b)

-- Three --
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty

  Three a b f <*> Three a' b' c =
    Three (a `mappend` a') (b `mappend` b') (f c)

-- Three' --
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x

  Three' a f f' <*> Three' a' b b' =
    Three' (a `mappend` a') (f b) (f' b')

-- Four --
-- Four' --
-- PALLA

-- Combinations --

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
-- combos as bs cs = (,,) <$> as <*> bs <*> cs
