module ListExtractingExercises where

-- no1
myWords :: String -> [String]
myWords s =
  case droppedZeros of
    "" -> []
    _ -> takeWhile (/=' ') droppedZeros : myWords (dropWhile (/=' ') droppedZeros)
    where droppedZeros = dropWhile (==' ') s

-- no2
myLines :: String -> [String]
myLines s =
  case droppedNewLines of
    "" -> []
    _  -> takeWhile (/='\n') droppedNewLines : myLines (dropWhile (/='\n') droppedNewLines)
    where droppedNewLines = dropWhile (=='\n') s

-- no3
wordsWith :: (Eq a) => a -> [a] -> [[a]]
wordsWith x xs =
  case dropped of
    [] -> []
    _ -> takeWhile (/=x) dropped : wordsWith x (dropWhile (/=x) dropped)
    where dropped = dropWhile (==x) xs

myWords' :: String -> [String]
myWords' = wordsWith ' '

myLines' :: String -> [String]
myLines' = wordsWith '\n'
