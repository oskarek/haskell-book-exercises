module UsingMaybeApplicative where

-- import Control.Applicative

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName = fmap Name . validateLength 25

mkAddress :: String -> Maybe Address
mkAddress = fmap Address . validateLength 100

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson name addr =
  Person <$> mkName name <*> mkAddress addr
-- mkPerson name addr =
--   liftA2 Person (mkName name) (mkAddress addr)
