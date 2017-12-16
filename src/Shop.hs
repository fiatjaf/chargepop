{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Shop
    ( Shop(..)
    , getShop
    ) where

import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField

data Shop = Shop { email :: Text, referral :: Text, created_at :: Text }
  deriving (Show, Generic)
instance ToJSON Shop
instance FromRow Shop where
  fromRow = Shop <$> field <*> field <*> field

getShop :: Connection -> Text -> IO [Shop]
getShop conn token =
  query conn "\
\ select email, ''::text, to_char(created_at, 'YYYY-MM-DDTHH24:MI:SS.MSZ') \
\ from shops \
\ where token = ? \
\ " (Only token)

parseToken :: Maybe Text -> Text
parseToken Nothing = ""
parseToken (Just auth) = auth
