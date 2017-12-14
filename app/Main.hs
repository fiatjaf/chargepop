{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Control.Monad.IO.Class
import Data.Text.Internal.Lazy (Text)
import Web.Scotty
import System.Environment
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple

main :: IO ()
main = do
  connString <- getEnv "DATABASE_URL"
  db <- connectPostgreSQL $ pack connString

  putStrLn "Starting server..."
  scotty 3000 $ server db

server :: Connection -> ScottyM()
server db = do
  get "/ping" $ do
    res <- liftIO ( query_ db "select email from shops where token = '9384yoisehrf9w38g5'" :: IO [Only Text] )
    case res of
      [] -> text "not found"
      (Only x:_) -> text x
  get "/identify" $ do
    text ""
  post "/identify" $ do
    text ""
  get "/spend" $ do
    text ""
  post "/spend" $ do
    text ""
  

data User = User { userId :: Text, userEmail :: Text } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User
