module Reverse
    ( rvrs
    ) where

-- PROBLEM:: can't use more than one sep in a row
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = split []
  where
    split acc _ [] = reverse $ dropWhile (==[]) $ map reverse acc
    split acc sep (h:t) = if h == sep
      then split ([]:acc) sep t
      else case acc of
        [] -> split ([h]:acc) sep t
        (aH:aT) -> split ((h:aH):aT) sep t

intersperse :: a -> [a] -> [a]
intersperse sep (h:h2:t) = h : sep : intersperse sep (h2:t)
intersperse _ l = l

-- skulle också kunna skrivas:
-- rvrs = concat . intersperse ' ' . reverse . splitOn ' '
rvrs :: String -> String
rvrs = unwords . reverse . words
