module StringVsText where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as SIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

dictWords :: IO String
dictWords =
  SIO.readFile "/usr/share/dict/words"

dictWordsT :: IO T.Text
dictWordsT =
  TIO.readFile "/usr/share/dict/words"

dictWordsTL :: IO TL.Text
dictWordsTL =
  TLIO.readFile "/usr/share/dict/words"

mainStringVsText :: IO ()
mainStringVsText = do
  replicateM_ 100 (dictWords >>= print)
  replicateM_ 100 (dictWordsT >>= TIO.putStrLn)
  replicateM_ 100 (dictWordsTL >>= TLIO.putStrLn)
