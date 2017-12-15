{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Shop
    ( Shop
    , getShop
    ) where

import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField

data Shop = Shop { email :: Text, token :: Text } deriving (Show, Generic)
instance ToJSON Shop
instance FromJSON Shop
instance FromRow Shop where
  fromRow = Shop <$> field <*> field

getShop :: Connection -> Text -> IO [Shop]
getShop conn token =
  query conn "select email, token from shops where token = ?" (Only token)
