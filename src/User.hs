{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module User
    ( User(..)
    , getUser, ensureUser
    ) where

import GHC.Generics
import Prelude hiding (id)
import Data.Text.Lazy (Text)
import Control.Monad
import Data.Aeson (ToJSON, FromJSON, parseJSON, withObject, (.:), (.!=), (.:?))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Data.Function

data User = User { key :: Text , email :: Text , balance :: Text }
  deriving (Show, Generic)
instance ToJSON User
instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
    <$> v .: "user"
    <*> v .: "email"
    <*> v .:? "balance" .!= ""
instance FromRow User where
  fromRow = User <$> field <*> field <*> field

getUser :: Connection -> Text -> Text -> IO [User]
getUser conn token key =
  query conn "\
\ select key, users.email, (balance / 10000000)::text from users \
\ inner join shops on shops.id = users.shop \
\ where key = ? and shops.token = ? \
\ " (key, token)

ensureUser :: Connection -> Text -> User -> IO Bool
ensureUser conn token (User key email _) =
  execute conn "\
\ insert into users (key, email, balance, shop) \
\ values (?, ?, 0, (select id from shops where token = ?)) \
\ " (key, email, token)
    & fmap (\c -> if c == 0 then False else True)
