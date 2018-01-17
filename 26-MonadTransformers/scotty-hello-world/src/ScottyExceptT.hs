{-# LANGUAGE OverloadedStrings #-}
module ScottyExceptT where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           Web.Scotty

-- bad --
param' :: Parsable a
       => Text -> ActionM (Either String a)
param' k =
  rescue (Right <$> param k)
          (const
            (return
              (Left $ "The key: "
                      ++ show k
                      ++ " was missing!")))

mainScottyE :: IO ()
mainScottyE = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    a <- param' "1"
    let a' = either (const 0) id a
    liftIO $ print (a :: Either String Int)
    liftIO $ print (a' :: Int)
    html $
      mconcat ["<h1>Scotty, ",
               beam,
               " me up!</h1>"]

-- good --
param'' :: Parsable a
       => Text -> ExceptT String ActionM a
param'' k = ExceptT $
  rescue (Right <$> param k)
          (const
            (return
              (Left $ "The key: "
                      ++ show k
                      ++ " was missing!")))

type Reco =
  (Integer, Integer, Integer, Integer)

tshow = TL.pack . show

mainScottyE' :: IO ()
mainScottyE' = scotty 3000 $
  get "/:word" $ do
    reco <- runExceptT $ do
      a <- param'' "1"
      liftIO $ print a
      b <- param'' "2"
      c <- param'' "3"
      d <- param'' "4"
      liftIO $ print b
      return ((a,b,c,d) :: Reco)
    case reco of
      Left e -> text (TL.pack e)
      Right r ->
        html $
          mconcat ["<h1>Scotty, ",
                   tshow r,
                   " me up!</h1>"]
