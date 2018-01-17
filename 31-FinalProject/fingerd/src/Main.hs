{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad                (forever)
import           Data.List                    (intersperse)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Typeable
import           Database.SQLite.Simple
import qualified Database.SQLite.Simple       as SQLite
import           Database.SQLite.Simple.Types
import           Network.Socket               hiding (recv)
import qualified Network.Socket               as Socket
import           Network.Socket.ByteString    (recv, sendAll)
import           Safe                         (headMay)
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

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser =
  "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"



allUsers :: Query
allUsers =
  "SELECT * from users"

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow =
  (Null, Text, Text, Text, Text, Text)

getUser :: Connection
        -> Text
        -> IO (Maybe User)
getUser conn username = do
  results <-
    query conn getUserQuery (Only username)
  if length results < 2
    then return $ headMay results
    else throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where meRow :: UserRow
        meRow =
          (Null, "callen", "/bin/zsh",
           "/home/callen", "Chris Allen",
           "555-123-4567")

returnUsers :: Connection
            -> Socket
            -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat (intersperse "\n" usernames)
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser User{..} =
  BS.concat $ zipWith (flip ($) . encodeUtf8)
    [ username, realName, homeDirectory, shell ]
    [ "Login: " >:< "\t\t\t\t", "Name: " >:< "\n"
    , "Directory: " >:< "\t\t\t", "Shell: " >:< "\n" ]
  where (>:<) left right a = BS.concat [left,a,right]

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username =
  getUser dbConn (T.strip username)
  >>= maybe (putStrLn errMess) (sendAll soc . formatUser)
  where errMess = "Couldn't find matching user for username: "
                    ++ show username

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" ->
      returnUsers dbConn soc
    name ->
      returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection
              -> Socket
              -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  Socket.close soc

main :: IO ()
main = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
    (Just (defaultHints
      {addrFlags = [AI_PASSIVE]}))
    Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
          Stream defaultProtocol
  Socket.bind sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  Socket.close sock
