module ExerciseEmbedded where
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = MaybeT $ ExceptT $ ReaderT $ return . const (Right (Just 1))

g :: () -> IO (Either String (Maybe Int))
g = return . const (Right (Just 1))

h :: ReaderT () IO (Either String (Maybe Int))
h = ReaderT g

i :: ExceptT String (ReaderT () IO) (Maybe Int)
i = ExceptT h

j :: MaybeT (ExceptT String (ReaderT () IO)) Int
j = MaybeT i
