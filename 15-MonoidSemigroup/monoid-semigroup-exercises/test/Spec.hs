import Test.QuickCheck
import SemigroupExercises

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc = Identity String
                  -> Identity String
                  -> Identity String
                  -> Bool
type TwoAssoc = Two String (Identity String)
             -> Two String (Identity String)
             -> Two String (Identity String)
             -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
-- type ValidationAssoc = Validation String Trivial
--                     -> Validation String Trivial
--                     -> Validation String Trivial
--                     -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  -- quickCheck (semigroupAssoc :: ValidationAssoc)
