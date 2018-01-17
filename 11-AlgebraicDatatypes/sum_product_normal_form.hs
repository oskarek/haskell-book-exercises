module SumProductNormalForm where

-- data Fiction = Fiction deriving Show
-- data NonFiction = NonFiction deriving Show
--
-- data BookType = FictionBook Fiction
--               | NonFictionBook NonFiction
--               deriving Show

type AuthorName = String

data Author = Fiction AuthorName
            | NonFiction AuthorName
            deriving (Eq, Show)


type Gardener = String

-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show
--
-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show
