{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Data.Text                    (Text)
import qualified Data.Text.IO                 as TIO
import           Data.Typeable
import           Database.SQLite.Simple
import qualified Database.SQLite.Simple       as SQLite
import           Database.SQLite.Simple.Types
import           System.Environment
import           System.IO
import           Text.RawString.QQ

data User =
  User {
      userId        :: Integer
    , username      :: Text
    , shell         :: Text
    , homeDirectory :: Text
    , realName      :: Text
    , phone         :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow User{..} =
    toRow (userId, username, shell, homeDirectory, realName, phone)

addUser :: Query
addUser = [r|
INSERT INTO users
  VALUES (?, ?, ?, ?, ?, ?)
|]

data TooManyArgs =
  TooManyArgs
  deriving (Eq, Show, Typeable)

instance Exception TooManyArgs

getDbFile :: IO String
getDbFile = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Type the path to the database file:"
      getLine
    [db] -> return db
    _ -> throwIO TooManyArgs

type UserRow =
  (Null, Text, Text, Text, Text, Text)

getUserInfo :: IO UserRow
getUserInfo =
  (,,,,,)
  <$> return Null
  <*> getInfo "Username: "
  <*> getInfo "Shell: "
  <*> getInfo "Home directory: "
  <*> getInfo "Real name: "
  <*> getInfo "Phone: "
  where getInfo s = putStr s >> TIO.getLine

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  conn <- getDbFile >>= open
  getUserInfo >>= execute conn addUser
  SQLite.close conn
  putStrLn "Successfully added user into database"
