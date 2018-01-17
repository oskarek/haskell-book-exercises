module Main where

import Hangman (freshPuzzle, randomWord', runGame)
import Data.Char (toLower)
import System.IO (hSetBuffering, stdout, BufferMode( NoBuffering ))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  fmap (freshPuzzle . map toLower) randomWord' >>= runGame
