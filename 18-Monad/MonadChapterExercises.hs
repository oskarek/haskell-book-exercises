module MonadChapterExercises where
-- import Data.Functor
import           Control.Monad

-- Nope --
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg

  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure

  _ >>= _ = NopeDotJpg

-- PhhhbttEither --
data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a)  = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
  pure = Left'

  Right' e <*> _ = Right' e
  _ <*> Right' e = Right' e
  Left' f <*> Left' a = Left' (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure

  Right' e >>= _ = Right' e
  Left' a  >>= f = f a

-- Identity --
newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity

  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure

  Identity a >>= f = f a

-- List --
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
  pure = (`Cons` Nil)

  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> ys =
    (f <$> ys) `append` (fs <*> ys)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x (append xs ys)

instance Monad List where
  return = pure

  Nil >>= _ = Nil
  Cons x xs >>= f = f x `append` (xs >>= f)

-- Write functions using Monad and Functor methods --
-- 1 --
j :: Monad m => m (m a) -> m a
j = join

-- 2 --
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3 --
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4 --
a' :: Monad m => m a -> m (a -> b) -> m b
a' = flip ap

-- 5 --
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = liftM2 (:) (f x) (meh xs f)

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id

myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus [] ys = ys
myPlusPlus (x:xs) ys = x : xs `myPlusPlus` ys
