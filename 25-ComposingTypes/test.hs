module Test where

newtype Three f g h a =
  Three { runThree :: f (g (h a)) }
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h)
      => Functor (Three f g h) where
        fmap f (Three fgha) =
          Three $ (fmap . fmap . fmap) f fgha
