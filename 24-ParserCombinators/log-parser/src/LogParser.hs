module LogParser where

import           Control.Applicative
import           Control.Monad       (void)
import           Data.Char           (digitToInt)
import           Data.List           (foldl', intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (isNothing)
import           Text.Trifecta

-- Data types --
data Time =
  Time Int Int
  deriving (Eq)

instance Show Time where
  show (Time h m) = intercalate ":" $ map show' [h,m]
    where show' x = if x < 10
                      then '0' : show x
                      else show x

type LogText = String

data LogEntry =
  LogEntry Time LogText
  deriving (Eq)

instance Show LogEntry where
  show (LogEntry t txt) = unwords [show t, txt]

type Date = String

data Day = Day Date [LogEntry] deriving (Eq, Show)

type Activity = String

newtype ActivityDurations =
  ActivityDurations { durations :: Map Activity Int }
  deriving (Eq, Show)

-- parsing --
clockBetween :: Int -> Int -> Parser Int
clockBetween = undefined

accumDigits :: [Char] -> Int
accumDigits = foldl' (\x d -> 10*x + digitToInt d) 0

hour :: Parser Int
hour =
  ((\h1 h2 -> accumDigits [h1,h2])
  <$> satisfyRange '0' '1'
  <*> satisfyRange '0' '9')
  <|> ((\h1 h2 -> accumDigits [h1,h2])
      <$> char '2'
      <*> satisfyRange '0' '3')

minute :: Parser Int
minute =
  (\h1 h2 -> accumDigits [h1,h2])
  <$> satisfyRange '0' '5'
  <*> satisfyRange '0' '9'

time :: Parser Time
time =
  Time <$> hour <* symbol ":" <*> minute

logText :: Parser LogText
logText =
  manyTill anyChar (try skipComments <|> void newline <|> eof)

skipComments :: Parser ()
skipComments =
  spaces *> string "--"
  *> skipMany (satisfy (/='\n'))
  *> (void newline <|> eof)

logEntry :: Parser LogEntry
logEntry = LogEntry <$> time <* spaces <*> logText

dayDate :: Parser Date
dayDate = symbol "#" *> spaces *> logText

day :: Parser Day
day = Day <$> dayDate <*> many logEntry

days :: Parser [Day]
days = sepBy day (some newline)

timeDiff :: Time -> Time -> Int
timeDiff (Time h m) (Time h' m') =
  let hDiff = (h' - h) * 60
      mDiff = m' - m
  in hDiff + mDiff

accumActivities :: Day -> ActivityDurations
accumActivities (Day _ es) =
  ActivityDurations $ foldr addDuration M.empty pairs
  where pairs = zipWith comb es (drop 1 es)
        comb (LogEntry t actvty) (LogEntry t' _) =
          (actvty, timeDiff t t')

        addDuration (k,v) = M.alter (Just . maybe v (+v)) k

activityDurations :: Parser ActivityDurations
activityDurations =
  ActivityDurations .
  foldr (M.unionWith (+) . durations . accumActivities) M.empty
  <$> days
