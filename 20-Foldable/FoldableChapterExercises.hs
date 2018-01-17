module FoldableChapterExercises where

import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (==a))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' f =
  if null' f
    then Nothing
    else Just $ foldr1 min f

minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
minimum'' = foldr min' Nothing
  where min' a Nothing = Just a
        min' a b = min a <$> b

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' f =
  if null' f
    then Nothing
    else Just $ foldr1 max f

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable t => t a -> Int
length' = foldr (\_ c -> c+1) 0

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap' id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

-- Write instances --
newtype Constant a b =
  Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

----------------------------

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z
  foldMap f = foldr (mappend . f) mempty

-----------------------------

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

---------------------------

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f z (Three' _ _ b') = f b' z

--------------------------

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

---------------------------

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF cond = foldMap (\a -> if cond a then pure a else mempty)
