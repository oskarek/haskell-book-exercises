{-# LANGUAGE TupleSections #-}
module Phone where
import Data.List
import Data.Char
import Data.Maybe (mapMaybe)

data PhoneKey = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Zero | Star | Hash
  deriving (Eq, Show)

data PhoneButton =  PhoneButton { key :: PhoneKey, chars :: String } deriving (Eq, Show)
newtype DaPhone = DaPhone [PhoneButton] deriving (Eq, Show)

phone :: DaPhone
phone = DaPhone
  [ PhoneButton One "1"
  , PhoneButton Two "ABC2"
  , PhoneButton Three "DEF3"
  , PhoneButton Four "GHI4"
  , PhoneButton Five "JKL5"
  , PhoneButton Six "MNO6"
  , PhoneButton Seven "PQRS7"
  , PhoneButton Eight "TUV8"
  , PhoneButton Nine "WXYZ9"
  , PhoneButton Zero "+ 0"
  , PhoneButton Star "^"
  , PhoneButton Hash ".,#" ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tested alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

type Presses = Integer

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing = Left a
maybeToEither _ (Just x) = Right x

keyAndPressesForChar :: Char -> PhoneButton -> Maybe (PhoneKey, Presses)
keyAndPressesForChar c (PhoneButton k cs) =
  let cLow = toLower c
      indexedChars = zip [1..] cs
  in  ((k ,) . fst) <$> find ((== cLow) . toLower . snd) indexedChars

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

reverseTaps :: DaPhone -> Char -> Either String [(PhoneKey, Presses)]
reverseTaps (DaPhone []) _ = Right []
reverseTaps (DaPhone buttons) c = do
  (k, presses) <- maybeToEither ("Can't produce char " ++ show c) $
                safeHead (mapMaybe (keyAndPressesForChar c) buttons)
  Right $ [(Star, 1) | isUpper c] ++ [(k, presses)]

cellPhonesDead :: DaPhone -> String -> Either String [(PhoneKey, Presses)]
cellPhonesDead p = fmap concat . mapM (reverseTaps p)

cellPhonesDead' :: DaPhone -> String -> Either String [(PhoneKey, Presses)]
cellPhonesDead' _ "" = Right []
cellPhonesDead' p (s:xs) = do
  taps <- reverseTaps p s
  rest <- cellPhonesDead' p xs
  return $ taps ++ rest

fingerTaps :: [(PhoneKey, Presses)] -> Presses
fingerTaps = sum . map snd

-- TODO --
-- charFingerTaps :: [(PhoneKey, Presses)] -> [(Char, Presses)]
-- charFingerTaps [] = []
-- charFingerTaps (x:xs) =
--   let partition ((== x) . toLower . fst . ) xs
--
-- mostPopularLetter :: String -> Either String (Char, Presses)
-- mostPopularLetter "" = Left "Empty string"
-- mostPopularLetter (s:xs) =  cellPhonesDead phone
