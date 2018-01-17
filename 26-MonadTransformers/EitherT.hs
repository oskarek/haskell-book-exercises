module EitherT where
import           Control.Applicative (liftA2)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT (fmap (fmap f) mea)

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  EitherT meefab <*> EitherT meea =
    EitherT $ liftA2 (<*>) meefab meea

instance Monad m => Monad (EitherT e m) where
  return = pure

  EitherT meea >>= f =
    EitherT $ meea >>= either (pure . Left) (runEitherT . f)

swapEither :: Either e a -> Either a e
swapEither = either Right Left

swapEitherT :: Monad m
            => EitherT e m a
            -> EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEitherT

eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT fa fb (EitherT ma) = ma >>= either fa fb
