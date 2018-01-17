module Print2 where

main :: IO ()
main = do
  putStrLn "Count to four for me:"
  putStr   "one, two"
  putStr   ", three, and"
  putStrLn " four!"

main2 :: IO ()
main2 = putStrLn "Count to four for me:"
  >> putStr "one, two, three, and four\n"

main3 :: IO ()
main3 = putStrLn "Count to four for me:"
  >>= (\_ -> putStr "one, two, three, and four\n")
