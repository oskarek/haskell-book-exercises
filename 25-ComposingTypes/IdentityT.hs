module IdentityT where

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity

  Identity f <*> d = fmap f d

instance Applicative m => Applicative (IdentityT m) where
  pure = IdentityT . pure

  IdentityT mab <*> IdentityT ma =
    IdentityT (mab <*> ma)

instance Monad Identity where
  return = pure

  Identity a >>= f = f a

instance Monad m => Monad (IdentityT m) where
  return = pure

  IdentityT ma >>= f =
    IdentityT $ ma >>= (runIdentityT . f)
