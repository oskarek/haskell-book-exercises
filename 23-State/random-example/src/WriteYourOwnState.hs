{-# LANGUAGE InstanceSigs #-}

module WriteYourOwnState where

import           Control.Arrow

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ (\(a,s) -> (f a, s)) . g

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi ((,) a)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  Moi f <*> Moi g =
    Moi $ g' . f
    where g' (f',s) = let (a,s') = g s
                      in (f' a,s')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  Moi f >>= g =
    Moi $ g' . f
    where g' (a, s) = let Moi g'' = g a
                      in g'' s
