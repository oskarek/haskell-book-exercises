module Bottom where

f :: Bool -> Int
f True = error "blah"
f False = 0

fWithMaybe :: Bool -> Maybe Int
fWithMaybe False = Just 0
fWithMaybe True = Nothing
