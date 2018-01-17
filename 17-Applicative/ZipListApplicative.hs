module ZipListApplicative where

-- ZipList --

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
  pure = ZipList' . pure

  ZipList' fs <*> ZipList' xs =
    ZipList' $ zipWith ($) fs xs
