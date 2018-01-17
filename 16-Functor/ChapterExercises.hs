{-# LANGUAGE FlexibleInstances #-}
module ChapterExercises where

-- REARRANGE --

-- 1 Sum a b --
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- 2 Company a b c --
data Company a c b =
    DeepBlue a c
  | Something b
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3 More a b --
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- WRITE --

-- 1 Quant --
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2 K --
newtype K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K b) = K b

-- 3 Flip --
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

-- EvilGoteeConst --
newtype EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- LiftItOut --
newtype LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f' (LiftItOut f) = LiftItOut (fmap f' f)

-- Parappa --
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f' (DaWrappa f g) =
    DaWrappa (fmap f' f) (fmap f' g)

-- IgnoreOne --
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f' (IgnoringSomething f g) =
    IgnoringSomething f (fmap f' g)

-- Notorious --
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious g g' g'') =
    Notorious g g' (fmap f g'')

-- List --
data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Goatlord --
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x)       = OneGoat (f x)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a)
                                       (fmap f b)
                                       (fmap f c)
