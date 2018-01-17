module DatabaseProcessing where

import Data.Time
import Data.Foldable

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1)
                    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1)
                    (secondsToDiffTime 34123))
  ]

flatMap :: (a -> Maybe b) -> [a] -> [b]
flatMap f = concatMap (toList . f)

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = flatMap dateOrNothing
  where dateOrNothing (DbDate d) = Just d
        dateOrNothing _          = Nothing

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = flatMap numberOrNothing
  where numberOrNothing (DbNumber n) = Just n
        numberOrNothing _            = Nothing

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = let nums = filterDbNumber db
           in  fromIntegral (sum nums) / fromIntegral (length nums)
