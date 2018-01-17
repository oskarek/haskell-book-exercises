{-# LANGUAGE OverloadedStrings #-}

module HitCount where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Internal.Types
import           Web.Scotty.Trans

data Config =
  Config {
    -- that's one, one click!
    -- two..two clicks!
    -- Three BAEUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let m' = M.alter (Just . maybe 1 (+1)) k m
      Just i = M.lookup k m'
  in (m',i)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    Config countsRef prfx <- lift ask
    counts <- lift . lift $ readIORef countsRef
    let key' = mappend prfx unprefixed
        (counts', newInteger) = bumpBoomp key' counts
    lift . lift $ writeIORef countsRef counts'
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack (show newInteger)
              , "</h1>"
              ]

mainHitCount :: IO ()
mainHitCount = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR m = runReaderT m config
  scottyT 3000 runR app
