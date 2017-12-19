{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module Main where

import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import Web.Scotty
import System.Environment
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status
import Data.Aeson (ToJSON, toJSON, object)
import Data.Aeson.Types ((.=))
import Data.Monoid ((<>))

import Shop (Shop(..), getShop, parseToken)
import User (User(..), getUser, ensureUser)

main :: IO ()
main = do
  connString <- getEnv "DATABASE_URL"
  db <- connectPostgreSQL $ pack connString

  putStrLn "Starting server..."
  scotty 3000 $ server db

server :: Connection -> ScottyM()
server db = do
  get "/ping" $ do
    token <- getToken
    shops <- liftIO $ getShop db token
    case shops of
      [] -> do
        status unauthorized401
        json $ Response False "Unauthorized. Access token is invalid"
      (shop : _) ->
        json $ ShopResponse shop
  get "/identify" $ do
    token <- getToken
    key <- param "user"
    users <- liftIO $ getUser db token key
    case users of
      [] -> do
        status notFound404
        json $ Response False "Match error: Failed Match.Where validation in field user"
      (user : _) ->
        json $ UserResponse user
  post "/identify" $ do
    token <- getToken
    (u :: User) <- jsonData
    created <- liftIO $ ensureUser db token u
    json $ Response True ("User successfully " <> if created then "created" else "updated") 
  get "/spend" $ do
    text ""
  post "/spend" $ do
    text ""

getToken :: ActionM Text
getToken = fmap parseToken $ header "Authorization"


data ShopResponse = ShopResponse { shop :: Shop }
instance ToJSON ShopResponse where
  toJSON (ShopResponse shop) =
    object
      [ "message" .= ("Authorized. Token is valid" :: Text)
      , "dev" .= shop
      , "status" .= ("success" :: Text)
      ]

data UserResponse = UserResponse { user :: User }
instance ToJSON UserResponse where
  toJSON (UserResponse (User key email balance)) =
    object
      [ "user" .= object
        [ "key" .= key
        , "email" .= email
        , "balances" .= object
          [ "available" .= balance
          , "current" .= balance
          ]
        , "link" .= ("" :: Text)
        ]
      , "status" .= ("success" :: Text)
      ]

data Response = Response { success :: Bool, message :: Text }
instance ToJSON Response where
  toJSON (Response success message) =
    object
      [ "message" .= message
      , "status" .= (if success then "success" else "error" :: Text)
      ]
