module Main where

import           Control.Exception
import           System.Environment

willIFail :: Integer
          -> IO (Either ArithException ())
willIFail denom =
  try $ print (5 `div` denom)

onlyReportError :: Show e
                => IO (Either e a)
                -> IO ()
onlyReportError action = action
  >>= either print (const $ return ())

testDiv :: String -> IO ()
testDiv d =
  onlyReportError $ willIFail (read d)

main :: IO ()
main = getArgs
  >>= mapM_ testDiv
