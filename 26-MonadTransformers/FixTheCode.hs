module FixTheCode where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard (isValid v)
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  putStrLn $ maybe "MOAR EXCITE" ("Good, was very excite: " ++) excite
