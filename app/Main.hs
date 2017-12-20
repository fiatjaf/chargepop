{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module Main where

import Control.Monad.IO.Class
import Data.Text.Lazy as T (Text, pack)
import Web.Scotty
import System.Environment
import Data.ByteString.Char8 as C8 (pack)
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Data.Aeson (ToJSON, toJSON, object)
import Data.Aeson.Types ((.=))
import Data.Monoid ((<>))

import Shop (Shop(..), getShop, parseToken)
import User (User(..), Spend(..), getUser, ensureUser, spend, getSpends)

main :: IO ()
main = do
  connString <- getEnv "DATABASE_URL"
  db <- connectPostgreSQL $ C8.pack connString

  putStrLn "Starting server..."
  scotty 3000 $ do
    middleware logStdoutDev
    server db

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
    token <- getToken
    key <- param "user"
    -- gte <- param "gte"
    -- lte <- param "lte"
    spends <- liftIO $ getSpends db token key
    json spends
    
  post "/spend" $ do
    token <- getToken
    key <- param "user"
    amount <- param "amount"
    desc <- param "description"
    amt <- liftIO $ spend db token key (amount, desc)
    json $ Response True (T.pack $ show amt <> " credits were spent")


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

data SpendsResponse = SpendsResponse { spends :: [Spend] }
instance ToJSON SpendsResponse where
  toJSON (SpendsResponse spends) =
    object
      [ "spends" .= toJSON spends
      , "status" .= ("success" :: Text)
      ]


data Response = Response { success :: Bool, message :: Text }
instance ToJSON Response where
  toJSON (Response success message) =
    object
      [ "message" .= message
      , "status" .= (if success then "success" else "error" :: Text)
      ]
