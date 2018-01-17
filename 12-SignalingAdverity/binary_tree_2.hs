module BinaryTree2 where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (l,b,r) -> Node (unfold f l) b (unfold f r)

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe p a = if p a then Just a else Nothing

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  let f = fmap (\x -> (x+1,x,x+1)) . toMaybe (< n)
  in  unfold f 0
