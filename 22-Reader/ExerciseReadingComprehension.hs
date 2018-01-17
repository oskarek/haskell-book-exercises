{-# LANGUAGE InstanceSigs #-}

module ExerciseReadingComprehension where

newtype Reader r a =
  Reader { runReader :: r -> a }

-- liftA2 --
myLiftA2 :: Applicative f
         => (a -> b -> c)
         -> f a
         -> f b
         -> f c
myLiftA2 f t t' = f <$> t <*> t'

-- asks --
asks :: (r -> a) -> Reader r a
asks = Reader

-- Reader applicative --
instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader r) <*> (Reader r') =
    Reader $ \e -> r e (r' e)

instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  Reader r >>= f =
    Reader $ \e -> runReader (f (r e)) e

-- Doggies --
newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy

getDogRM'' :: Reader Person Dog
getDogRM'' =
  Reader dogName >>=
    \name -> Reader address >>=
      \addy -> return $ Dog name addy
