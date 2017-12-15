{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module User
    ( User
    ) where

import GHC.Generics
import Prelude hiding (id)
import Data.Text.Lazy (Text)
import Data.Aeson (FromJSON, ToJSON)

data User = User { id :: Text, email :: Text } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User
