{-# LANGUAGE TupleSections #-}
module Chapter12Exercises where

-- 1 --
notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

-- It's only natural --
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n = case compare n 0 of
  LT -> Nothing
  EQ -> Just Zero
  GT -> Just . Succ . fromMaybe Zero $ integerToNat (n-1)

-- small Maybe library --
-- 1
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

-- 3
fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[])

-- 5
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

catMaybes' :: [Maybe a] -> [a]
catMaybes' []     = []
catMaybes' (x:xs) = case x of
  Nothing -> catMaybes' xs
  Just x' -> x' : catMaybes' xs

-- 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (x:xs) = do
  x' <- x
  xs' <- flipMaybe xs
  return $ x' : xs'

-- small Either library --
-- 1
eitherToLeftList :: Either a b -> [a]
eitherToLeftList = either' (:[]) (const [])

eitherToRightList :: Either a b -> [b]
eitherToRightList = either' (const []) (:[])

lefts'' :: [Either a b] -> [a]
lefts'' []          = []
lefts'' (Left x:xs) = x : lefts'' xs
lefts'' (_:xs)      = lefts'' xs

lefts' :: [Either a b] -> [a]
lefts' = foldr ((++) . eitherToLeftList) []

rights' :: [Either a b] -> [b]
rights' = foldr ((++) . eitherToRightList) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _ (Left _)  = Nothing
eitherMaybe f (Right a) = Just $ f a

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' lf _ (Left a)  = lf a
either' _ rf (Right b) = rf b

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = either' (const Nothing) (Just . f)

-- unfolds --
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing -> []
  Just (x,x')  -> x : myUnfoldr f x'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))
