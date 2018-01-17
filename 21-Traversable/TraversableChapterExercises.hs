module TraversableChapterExercises where

import           Control.Applicative (liftA2, liftA3)

-- Identity --
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

-- Constant --
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = Constant <$> pure a

-- Maybe --
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

-- List --
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Traversable List where
  -- traverse _ Nil = pure Nil
  -- traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs) -- Cons <$> f x <*> traverse f xs
  traverse f = foldr cons' (pure Nil)
    where cons' x = liftA2 Cons (f x)

-- Three --
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- Pair --
data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f z (Pair _ b) = f b z

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

-- Big --
data Big a b =
  Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b `mappend` f b'
  foldr f z (Big _ b b') = f b (f b' z)

instance Traversable (Big a) where
  traverse f (Big a b b') = liftA2 (Big a) (f b) (f b') -- Big a <$> f b <*> f b'

-- Bigger --
data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b `mappend` f b' `mappend` f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = liftA3 (Bigger a) (f b) (f b') (f b'')

-- S --
data S n a =
  S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S t a) = S (fmap f t) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S t a) = f a `mappend` foldMap f t

instance Traversable n => Traversable (S n) where
  traverse f (S t a) = liftA2 S (traverse f t) (f a) -- S <$> traverse f t <*> f a

-- Tree --
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

  foldr _ z Empty = z
  foldr f z (Leaf x) = f x z
  foldr f z (Node l x r) =
    let r' = foldr f z r
    in  foldr f (f x r') l

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) = liftA3 Node (traverse f l) (f x) (traverse f r)
