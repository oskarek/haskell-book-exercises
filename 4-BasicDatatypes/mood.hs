module Mood where
import Data.List

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (h:t) = let (less, greater) = partition (<h) t
                   in quickSort less ++ [h] ++ quickSort greater
