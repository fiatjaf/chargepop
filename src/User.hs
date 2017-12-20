{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module User
    ( User(..)
    , Spend(..)
    , getUser, ensureUser, spend, getSpends
    ) where

import GHC.Generics
import Prelude hiding (id)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Read
import Data.Aeson (ToJSON, FromJSON, parseJSON, withObject, (.:), (.!=), (.:?))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow()
import Database.PostgreSQL.Simple.ToField()
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField()
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

data Spend = Spend { amount :: Double, description :: Text, spent_at :: Text }
  deriving (Show, Generic)
instance ToJSON Spend
instance FromRow Spend where
  fromRow = Spend <$> field <*> field <*> field

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
\ on conflict do nothing \
\ " (key, email, token)
    & fmap (\c -> if c == 0 then False else True)

spend :: Connection -> Text -> Text -> (Text, Text) -> IO Double
spend conn token key (amount, desc) = do
  execute conn "\
\ with inserted as ( \
\   insert into spends (shop, amount, description, user_key) \
\   values ((select id from shops where token = ?), (? * 10000000), ?, ?) \
\   returning * \
\ ), \
\ subtracted as ( \
\   update users set balance = balance - (select amount from inserted) \
\   where key = (select user_key from inserted) \
\     and shop = (select shop from inserted) \
\ ) \
\ delete from spends \
\ where shop = (select shop from inserted) \
\   and timestamp < (select timestamp from inserted) - interval '3 months' \
\ " (token, parseDouble amount, desc, key)
  & fmap (\_ -> parseDouble amount)

getSpends :: Connection -> Text -> Text -> IO [Spend]
getSpends conn token key =
  query conn "\
\ select (amount::float / 10000000), description, timestamp::text from spends \
\ inner join users on user_key = users.key \
\ inner join shops on users.shop = shops.id \
\ where key = ? and shops.token = ? \
\ " (key, token)

parseDouble :: Text -> Double
parseDouble text =
  case double text of
    Left _ -> 0
    Right (n, _) -> n
