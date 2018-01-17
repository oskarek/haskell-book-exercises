{-# LANGUAGE OverloadedStrings #-}
module HelloWorld where

-- import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid            (mconcat)
import           Web.Scotty

mainScotty = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    liftIO (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ",
                    beam,
                    " me up!</h1>"]
