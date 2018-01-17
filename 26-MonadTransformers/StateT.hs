module StateT where
import           Control.Arrow (first)

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ fmap (first f) . runStateT m

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
