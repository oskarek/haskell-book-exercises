{-# LANGUAGE TupleSections #-}
module MonadTransInstances where
import           Control.Applicative       (liftA2)
import           Control.Monad.Trans.Class

-- EitherT --
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

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

-- StateT --
newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ fmap (\(a,s)->(f a,s)) . runStateT m

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a,s)

  StateT mf <*> StateT m =
    StateT $ \s -> do
      (f,s') <- mf s
      (a,s'') <- m s'
      return (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure

  StateT sma >>= f =
    StateT $ \s -> do
      (a,s') <- sma s
      runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift = StateT . \m s -> fmap (, s) m
