module Morse
       ( Morse
       , charToMorse
       , morseToChar
       , stringToMorse
       , letterToMorse
       , morseToLetter
       ) where

import qualified Data.Map as M

type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse = M.fromList [
    ('a', ".-")
  , ('b', "-...")
  , ('c', "-.-.")
  , ('d', "-..")
  , ('e', ".")
  , ('f', "..-.")
  , ('g', "--.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".---")
  , ('k', "-.-")
  , ('l', ".-..")
  , ('m', "--")
  , ('n', "-.")
  , ('o', "---")
  , ('p', ".--.")
  , ('q', "--.-")
  , ('r', ".-.")
  , ('s', "...")
  , ('t', "-")
  , ('u', "..-")
  , ('v', "...-")
  , ('w', ".--")
  , ('x', "-..-")
  , ('y', "-.--")
  , ('z', "--..")
  , ('1', ".----")
  , ('2', "..---")
  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-....")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , ('0', "-----")
  ]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldWithKey (flip M.insert) M.empty
                              letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.findWithDefault Nothing c $ fmap Just letterToMorse

-- charToMorse' :: Char -> Maybe Morse
-- charToMorse' c = M.foldWithKey (\k a z -> if k == c then Just a else z) Nothing letterToMorse
--
-- charToMorse'' :: Char -> Maybe Morse
-- charToMorse'' c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter