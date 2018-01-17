module StandardFunctions where

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny p (x:xs) = p x || myAny p xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ []     = True
myAll p (x:xs) = p x && myAll p xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs) = e == x || myElem e xs

myElemWithAny :: Eq a => a -> [a] -> Bool
myElemWithAny e = myAny (==e)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse2 :: [a] -> [a]
myReverse2 = foldl (flip (:)) []

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squish2 :: [[a]] -> [a]
squish2 = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x:y:xs) = myMaximumBy f (max' : xs)
  where max' = case f x y of
                  GT -> x
                  _  -> y

myMaximumBySafe :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBySafe cmp = foldl max' Nothing
  where max' x y = case x of
          Nothing -> Just y
          Just x' -> case cmp x' y of
            GT -> Just x'
            _ -> Just y
