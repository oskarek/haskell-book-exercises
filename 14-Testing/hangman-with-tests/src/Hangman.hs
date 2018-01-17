module Hangman where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.List (intersperse, group, sort)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO (hSetBuffering, stdout, BufferMode( NoBuffering ))

newtype WordList = WordList [String] deriving (Eq, Show)
-- type WordList = [String]

allWords :: IO WordList
allWords =
  WordList . lines <$> readFile "data/dict.txt"

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter (gameLength . length) aw)
  where gameLength n =
          n >= minWordLength && n <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) =
  (wl !!) <$> randomRIO (0, length wl - 1)

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]
              deriving Eq

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wrd _ _) c = c `elem` wrd

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ show guess
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
                \ character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word,\
                \ filling in the word accordingly"
      return $ fillInCharacter puzzle guess
    (False, _) -> do
      putStrLn "This character wasn't in\
                \ the word, try again"
      return $ fillInCharacter puzzle guess

failedGuesses :: Puzzle -> Int
failedGuesses (Puzzle _ filledIn guessed) =
  let totalGuesses = length guessed
      correctGuesses = length $ group $ sort $ catMaybes filledIn
  in totalGuesses - correctGuesses

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledIn guessed) =
  when (failedGuesses (Puzzle wordToGuess filledIn guessed) > 7) $
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $
    do putStrLn "You win"
       exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                    \ be a single character"
