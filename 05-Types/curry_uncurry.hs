module CurryUncurry where

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)
