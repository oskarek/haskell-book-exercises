module ChapterExercises where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor               ((<$))

-- rDec --
rDec :: Num a => Reader a a
rDec = reader (subtract 1)

-- rShow --
rShow :: Show a => Reader a String
rShow = reader show

-- rPrintAndInc --
rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = do
  a <- ask
  liftIO (putStrLn $ "Hi: " ++ show a)
  return (a+1)

-- sPrintIncAccum --
sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = do
  a <- get
  liftIO (putStrLn $ "Hi: " ++ show a)
  show a <$ put (a+1)
