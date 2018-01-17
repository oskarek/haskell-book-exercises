import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    oneof $ map return [Fools, Twoo]

instance Monoid Bull where
  mempty = Fools
  _ `mappend` _ = Fools

instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monoid Twoo
  quickBatch $ applicative [("b", "w", 1 :: Int)]
