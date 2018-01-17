module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count+1)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "should evaluate 1 + 1 as greater than 1" $
      (1 + 1 :: Integer) > 1 `shouldBe` True
    it "should evaluate 2 + 2 as equal to 4" $
      (2 + 2 :: Integer) `shouldBe` 4
    it "should always evaluate x + 1 to greater than x" $
      property $ \x -> x + 1 > (x :: Int)
  describe "dividedBy" $ do
    it "should evaluate 15 dividedBy 3 as 5" $
      ((15 :: Integer) `dividedBy` 3) `shouldBe` (5, 0)
    it "should evaluate 22 dividedBy 5 as 4 remainder 2" $
      ((22 :: Integer) `dividedBy` 5) `shouldBe` (4, 2)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
               Gen (a,b,c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]
