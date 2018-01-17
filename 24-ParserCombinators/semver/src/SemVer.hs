module SemVer where

import           Control.Applicative
import           Data.Char           (digitToInt)
import           Data.Functor        ((<$))
import           Data.List           (find, foldl', intersperse)
import           Data.Maybe          (fromMaybe)
import           Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

-- parser --

posDigit :: Parser Char
posDigit = satisfyRange '1' '9'

posInt :: Parser Integer
posInt =
  read <$> liftA2 (:) posDigit (many digit)

nonNegInt :: Parser Integer
nonNegInt =
  try posInt
  <|> toInteger . digitToInt <$> char '0'

nos :: Parser NumberOrString
nos =
  try (NOSI <$> integer <* notFollowedBy alphaNum)
  <|> NOSS <$> some alphaNum

preReleaseInfo :: Parser Release
preReleaseInfo = option [] $ symbol "-" *> nos `sepBy1` symbol "."

metaData :: Parser Metadata
metaData = option [] $ symbol "+" *> nos `sepBy1` symbol "."

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer
  <$> nonNegInt
  <* symbol "."
  <*> nonNegInt
  <* symbol "."
  <*> nonNegInt
  <*> preReleaseInfo
  <*> metaData

-- ord instance --
instance Ord NumberOrString where
  NOSS str `compare` NOSS str' =
    str `compare` str'
  NOSI num `compare` NOSI num' =
    num `compare` num'
  NOSI _ `compare` NOSS _ = LT
  NOSS _ `compare` NOSI _ = GT

instance Ord SemVer where
  compare = compareSemVers

compareSemVers :: SemVer -> SemVer -> Ordering
compareSemVers (SemVer mj mn p pre _)
               (SemVer mj' mn' p' pre' _) = mconcat comps
    where compVersions = zipWith compare [mj,mn,p] [mj',mn',p']
          compPre = zipWith compare pre pre'
          compPreLen
            | null pre  = if null pre' then EQ else GT
            | null pre' = LT
            | otherwise = length pre `compare` length pre'
          comps = concat [compVersions, compPre, [compPreLen]]
