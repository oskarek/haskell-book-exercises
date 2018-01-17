module UsingMaybeMonad where

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n = case compare n 0 of
  LT -> Nothing
  _  -> Just n

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bless" && w > 499
    then Nothing
    else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
-- mkSphericalCow name' age' weight' =
--   let cow = Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'
--   in case cow of
--     Nothing -> Nothing
--     Just cow' -> weightCheck cow'
-- mkSphericalCow name' age' weight' =
--   noEmpty name' >>=
--     \name'' ->
--     noNegative age' >>=
--       \age'' ->
--       noNegative weight' >>=
--         \weight'' ->
--         weightCheck (Cow name'' age'' weight'')
mkSphericalCow name' age' weight' = do
  n <- noEmpty name'
  a <- noNegative age'
  w <- noNegative weight'
  weightCheck (Cow n a w)
