module TestFunct where

-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y
--
-- ((m -> n) -> f m -> f n) -> ((x -> y) -> g x -> g y) -> (x -> y) -> f m -> f n
--
-- ((g x -> g y) -> f (g x) -> f (g y)) -> ((x -> y) -> g x -> g y) -> (x -> y) -> f (g x) -> f (g y)
--
-- (Char -> String) -> (Int -> Char) -> Int -> String
