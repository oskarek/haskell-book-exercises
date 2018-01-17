module Print3Flipped where

myGreeting :: String
myGreeting = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting =
          (++) hello ((++) " " world)

putStrLnWithSpace :: String -> String -> IO ()
putStrLnWithSpace = \x ->
  (\y -> putStrLn $ (++) x $ (++) " " y)

putStrLnWithSpace2 :: String -> String -> IO ()
putStrLnWithSpace2 x = putStrLn . (++) x . (++) " "

putStrLnWithSpace3 :: String -> String -> IO ()
putStrLnWithSpace3 x y = putStrLn (x ++ " " ++ y)
