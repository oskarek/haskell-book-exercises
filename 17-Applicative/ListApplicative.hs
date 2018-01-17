module ListApplicative where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure = (`Cons` Nil)
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> as = (f <$> as) `append` (fs <*> as)

append :: List a -> List a -> List a
Nil `append` ys = ys
(Cons x xs) `append` ys = Cons x (xs `append` ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f
