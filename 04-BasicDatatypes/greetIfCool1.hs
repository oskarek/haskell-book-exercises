module GreetIfCool1 where

greetIfCool1 :: String -> IO ()
greetIfCool1 coolness =
  if cool
    then putStrLn "eyyyyy. What's shakin'?"
    else putStrLn "pshhhh."
  where
    cool = coolness == "downright frosty yo"
