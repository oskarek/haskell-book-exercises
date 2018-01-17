module ExerciseEvaluate where

one :: Integer
one = one

a,b,c,d :: Integer
a = const one False
b = (\x _ -> x) one False
c = (\_ -> one) False
d = one

a2, b2, c2, d2 :: Integer
a2 = const undefined one
b2 = (\x _ -> x) undefined one
c2 = (\_ -> undefined) one
d2 = undefined

a3, b3, c3, d3, e3, f3, g3 :: Integer
a3 = flip const undefined one
b3 = (\f x y -> f y x) (\x _ -> x) undefined one
c3 = (\x y -> (\x1 _ -> x1) y x) undefined one
d3 = (\y -> (\x1 _ -> x1) y undefined) one
e3 = (\x1 _ -> x1) one undefined
f3 = (\_ -> one) undefined
g3 = one

a4, b4, c4, d4, e4, f4, g4 :: Integer
a4 = flip const one undefined
b4 = (\f x y -> f y x) (\x _ -> x) one undefined
c4 = (\x y -> (\x1 _ -> x1) y x) one undefined
d4 = (\y -> (\x1 _ -> x1) y one) undefined
e4 = (\x1 _ -> x1) undefined one
f4 = (\_ -> undefined) one
g4 = undefined

a5, b5, c5, d5 :: Integer
a5 = const undefined undefined
b5 = (\x _ -> x) undefined undefined
c5 = (\_ -> undefined) undefined
d5 = undefined

-- foldr const 'z' ['a'..'e']
-- (\f z (x:xs) -> x `f` (recurse.. z xs)) (\x _ -> x) 'z' ['a'..'e']
-- (\z (x:xs) -> (\x1 _ -> x1) x (recurse.. z xs)) 'z' ['a'..'e']
-- (\(x:xs) -> (\x1 _ -> x1) x (recurse.. 'z' xs)) ['a'..'e']
-- (\x1 _ -> x1) 'a' (recurse.. 'z' ['b'..'e'])
-- (\_ -> 'a') (recurse.. 'z' ['b'..'e'])
-- 'a'
