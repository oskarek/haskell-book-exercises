module OptionalMonoid where

data Optional a = Some a | None deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = None
  None `mappend` b = b
  a `mappend` None = a
  Some a `mappend` Some b = Some (a `mappend` b)
