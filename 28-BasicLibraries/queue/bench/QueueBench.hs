import Criterion.Main
import Queue
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S

-- QUEUE --
pop' :: Queue a -> Queue a
pop' = maybe (Queue [] []) id . fmap snd . pop

queueFns :: [Queue Int -> Queue Int]
queueFns = cycle $ (replicate 100 pop') ++ (replicate 100 (push 1))

benchMark :: Queue Int -> Queue Int
benchMark q = foldr ($) q $ take 1000 queueFns

-- LIST --
popL :: [a] -> [a]
popL [] = []
popL [_] = []
popL (x:xs) = x : popL xs

pushL :: a -> [a] -> [a]
pushL = (:)

listFns :: [[Int] -> [Int]]
listFns = cycle $ (replicate 100 popL) ++ (replicate 100 (pushL 1))

benchMarkL :: [Int] -> [Int]
benchMarkL l = foldr ($) l $ take 1000 listFns

-- SEQUENCE --
popS :: Seq a -> Seq a
popS S.Empty = S.Empty
popS (xs :|> _) = xs

pushS :: a -> Seq a -> Seq a
pushS = (<|)

seqFns :: [Seq Int -> Seq Int]
seqFns = cycle $ (replicate 100 popS) ++ (replicate 100 (pushS 1))

benchMarkS :: Seq Int -> Seq Int
benchMarkS s = foldr ($) s $ take 1000 seqFns

-- MAIN --
main = defaultMain
  [ bench "push and pop queue" $
    nf benchMark (Queue [] (replicate 1000000 1))
  -- , bench "push and pop list" $
  --   nf benchMarkL (replicate 1000000 1)
  , bench "push and pop sequence" $
    nf benchMarkS (S.fromList (replicate 1000000 1))
  ]
