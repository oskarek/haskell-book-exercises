module TenPointTenChapterExercises where

-- WARM-UP AND REVIEW
-- 1 --
stops, vowels :: String
stops  = "pbtdkg"
vowels = "aeiou"

-- a
stopVowelStopCombs :: [String]
stopVowelStopCombs = [x : y : [z] | x <- stops, y <- vowels, z <- stops]

stopVowelStopCombsOnyAP :: [String]
stopVowelStopCombsOnyAP = ["ap" ++ [x] | x <- stops]

-- 2 --
type Sentence = String
avgWordLength :: Sentence -> Double
avgWordLength s = fromIntegral (sum . map length $ wds)
                  / fromIntegral (length wds)
  where wds = words s

-- REWRITING FUNCTIONS USING FOLDS
-- 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr ((||) . p) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr ((||) . (==e)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x z -> if p x then x : z else z) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp = foldr1 max'
  where max' x y = case cmp x y of
                     GT -> x
                     _ -> y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp = foldr1 min'
  where min' x y = case cmp x y of
                     GT -> y
                     _ -> x
