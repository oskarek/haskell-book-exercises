module BuildingFunctions where

addExclamation :: String -> String
addExclamation = (++"!")

indexFourString :: String -> String
indexFourString = (:[]) . (!!4)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = split []
  where
    split acc _ [] = reverse $ dropWhile (==[]) $ map reverse acc
    split acc delim (h:t) = if h == delim
      then split ([]:acc) delim t
      else case acc of
        [] -> split ([h]:acc) delim t
        (aH:aT) -> split ((h:aH):aT) delim t

lastWord :: String -> String
lastWord = last . splitOn ' '

thirdLetter :: String -> Char
thirdLetter = (!!2)

letterIndex :: Int -> Char
letterIndex = ("Curry is awesome"!!)

intersperse :: a -> [a] -> [a]
intersperse sep (h:h2:t) = h : sep : intersperse sep (h2:t)
intersperse _ l = l

rvrs :: String -> String
rvrs = concat . intersperse " " . reverse . splitOn ' '
