{-# LANGUAGE TupleSections #-}
module MonadIOInstances where
import           Control.Applicative    (liftA2)
import           Control.Monad.IO.Class
-- import Control.Monad.Trans.Maybe

-- MaybeT --
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT (fmap (fmap f) mma)

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure

  MaybeT mmab <*> MaybeT mma =
    MaybeT $ liftA2 (<*>) mmab mma

instance Monad m => Monad (MaybeT m) where
  return = pure

  MaybeT mma >>= f =
    MaybeT $ mma >>= maybe (pure Nothing) (runMaybeT . f)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = MaybeT . fmap Just . liftIO

-- ReaderT --
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ fmap (fmap f) rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure

  ReaderT rmab <*> ReaderT rma =
    ReaderT $ liftA2 (<*>) rmab rma

instance Monad m => Monad (ReaderT r m) where
  return = pure

  m >>= f = ReaderT $ \r -> do
              a <- runReaderT m r
              runReaderT (f a) r

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO

-- StateT --
newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ fmap (\(a,s) -> (f a, s)) . runStateT m

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

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = StateT . (\m s -> fmap (, s) m) . liftIO
