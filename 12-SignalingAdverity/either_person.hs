module EitherPerson where
-- import Control.Applicative
type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Age Name deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

ageOk :: Age -> Either [PersonInvalid] Age
ageOk age = if age >= 0 then Right age else Left [AgeTooLow]

nameOk :: Name -> Either [PersonInvalid] Name
nameOk name = if null name then Left [NameEmpty] else Right name

mkPerson :: Age -> Name -> ValidatePerson Person
mkPerson age name = mkPerson' (ageOk age) (nameOk name)

mkPerson' :: ValidatePerson Age
         -> ValidatePerson Name
         -> ValidatePerson Person
mkPerson' (Right age) (Right name) = Right $ Person age name
mkPerson' (Left ageErrors) (Left nameErrors) =
  Left $ ageErrors ++ nameErrors
mkPerson' (Left ageErrors) _  = Left ageErrors
mkPerson' _ (Left nameErrors) = Left nameErrors

-- mkPerson'' :: Age
--            -> Name
--            -> ValidatePerson Person
-- mkPerson'' age name = liftA2 Person (ageOk age) (nameOk name)
