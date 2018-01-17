module ListPatternMatching where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool a b = [a, b]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftOrd (succ a) b

eftOrd2 :: Ordering -> Ordering -> [Ordering]
eftOrd2 a b
  | a == b = [a]
eftOrd2 LT EQ = [LT, EQ]
eftOrd2 LT GT = [LT, EQ, GT]
eftOrd2 EQ GT = [EQ, GT]
eftOrd2 _ _   = []


-- Y COMBINATOR
y :: (t -> t) -> t
y f = f (y f)

almostFactorial :: Integral a => (a -> a) -> a -> a
almostFactorial _ 0 = 1
almostFactorial f n = n * f (n-1)

factorial :: Integral a => a -> a
factorial = y almostFactorial
