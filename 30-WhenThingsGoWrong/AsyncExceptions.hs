module Main where

-- we haven't explained this.
-- tough cookies.
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception
import           System.IO

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "test.dat" WriteMode
  threadDelay 50
  hPutStr h
    (replicate 1000000000 '0' ++ "abc")
  hClose h

data PleaseDie =
  PleaseDie
  deriving (Eq, Show)

instance Exception PleaseDie

main :: IO ()
main = do
  threadId <- forkIO (mask_ openAndWrite)
  threadDelay 1000
  throwTo threadId PleaseDie
