{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Queue where
import           Control.DeepSeq
import           GHC.Generics    (Generic, Generic1)

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show, Generic, Generic1, NFData, NFData1)

queue :: [a] -> [a] -> Queue a
queue enq []  = Queue [] (reverse enq)
queue enq deq = Queue enq deq

-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue enq deq) = queue (x:enq) deq

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue _ [])      = Nothing
pop (Queue enq (h:t)) = Just (h, queue enq t)
