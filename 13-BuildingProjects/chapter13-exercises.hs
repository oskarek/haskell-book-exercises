module Chapter13Exercises where

import System.IO (hSetBuffering, stdout, BufferMode( NoBuffering ))
import Control.Applicative (liftA2, (<*>))

type Name = String
type Age = Integer
type ValidatePerson a = Either PersonInvalid a

data Person = Person Age Name deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

ageOk :: Age -> Either PersonInvalid Age
ageOk age = if age >= 0 then Right age else Left AgeTooLow

nameOk :: Name -> Either PersonInvalid Name
nameOk name = if null name then Left NameEmpty else Right name

mkPerson :: Age -> Name -> ValidatePerson Person
mkPerson age name = pure Person <*> ageOk age <*> nameOk name
-- mkPerson age name = liftA2 Person (ageOk age) (nameOk name)

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Please typ in your name: "
  name <- getLine
  putStr "And your age please: "
  age <- read <$> getLine
  case mkPerson age name of
    Right (Person _ n) ->
      putStrLn $ "Yay! Successefully got a person: " ++ n
    Left err ->
      putStrLn $ "Oops! An error occurred: " ++ show err
