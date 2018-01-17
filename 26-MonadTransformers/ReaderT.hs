module ReaderT where
import           Control.Applicative (liftA2)

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
