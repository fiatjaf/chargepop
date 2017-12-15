{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import Web.Scotty
import System.Environment
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status
import Data.Aeson (ToJSON, toJSON, object)
import Data.Aeson.Types ((.=))

import Shop (Shop, getShop)
import User (User)

main :: IO ()
main = do
  connString <- getEnv "DATABASE_URL"
  db <- connectPostgreSQL $ pack connString

  putStrLn "Starting server..."
  scotty 3000 $ server db

server :: Connection -> ScottyM()
server db = do
  get "/ping" $ do
    token <- param "token"
    shops <- liftIO $ getShop db token
    case shops of
      [] -> do
        status status401
        text "token not registered"
      (shop : _) ->
        json $ ShopResponse shop
  get "/identify" $ do
    text ""
  post "/identify" $ do
    text ""
  get "/spend" $ do
    text ""
  post "/spend" $ do
    text ""


data ShopResponse = ShopResponse { shop :: Shop }
instance ToJSON ShopResponse where
  toJSON (ShopResponse shop) =
    object
      [ "message" .= ("Authorized. Token is valid" :: Text)
      , "dev" .= shop
      , "status" .= ("success" :: Text)
      ]

parseToken :: Maybe Text -> Text
parseToken Nothing = ""
parseToken (Just auth) = auth
