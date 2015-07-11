{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram where

import GHC.Generics hiding (from)
import Data.Aeson
import Token
import Control.Applicative
import Control.Monad (foldM)
import qualified Data.ByteString.Lazy as L

import Network.HTTP.Conduit (simpleHttp)

import Data.List (isInfixOf)
import Data.Char (toLower)
import Data.Text hiding (map, isInfixOf, toLower)

import Control.Concurrent

--base_message   = "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ chatid ++ "&text="

parseMessage :: Message -> IO ()
parseMessage m
  | has "no you" = send cid "No you."
  | otherwise    = return ()
  where f   = from m
        cid = chat_id $ chat m
        has = flip isInfixOf $ map toLower $ text m

send :: Int -> String -> IO ()
--send = putStrLn
send cid text = do
  simpleHttp $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show cid ++ "&text=" ++ text
  return ()

parseUpdate :: Int -> Update -> IO Int
parseUpdate n u = parseMessage (message u) >> return (update_id u)

loopForUpdates :: Int -> IO ()
loopForUpdates uid = do
  updates <- getUpdates uid
  print updates
  uid'    <- foldM parseUpdate uid updates
  threadDelay 1000000
  loopForUpdates (uid'+1)

-- TODO: Clean this shit up
getUpdates :: Int -> IO [Update]
getUpdates latest_uid = do
  json <- simpleHttp addr
  let d = eitherDecode json :: Either String TelegramResult
  case d of
    Left err -> putStrLn err >> return []
    Right ps -> if ok ps
                then return $ result ps
                else print "update not okay" >> return []
  where addr = "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ show latest_uid

-- | Telegram JSON objects
data TelegramResult 
  = TelegramResult 
  { ok     :: Bool
  , result :: [Update]
  } deriving (Show, Generic)

data Update
  = Update
  { update_id :: Int 
  , message   :: Message
  } deriving (Show, Generic)

data Message
  = Message
  { message_id :: Int
  , from       :: User
  , date       :: Int
  , text       :: String
  , chat       :: GroupChat
  } deriving (Show, Generic)

instance FromJSON TelegramResult
instance FromJSON Update
instance FromJSON Message

data User 
  = User 
  { user_id    :: Int
  , first_name :: String
  , last_name  :: Maybe Text
  , username   :: Maybe Text
  } deriving (Show)

-- | Explicity define because of the id fields
instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .:  "id"
         <*> v .:  "first_name"
         <*> v .:? "last_name"
         <*> v .:? "useriname"

data GroupChat
  = GroupChat
  { chat_id :: Int
  --, title   :: String
  } deriving (Show, Generic)

-- | Explicity define because of the id field
instance FromJSON GroupChat where
  parseJSON (Object v) =
    GroupChat <$> v .:  "id"
    --          <*> v .:  "title"


