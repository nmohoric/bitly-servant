{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Web.Bitly.User
       (
         UserLinkAPI
       ) where

import Control.Applicative
import Data.Aeson.Types
import GHC.Generics
import Servant.API

import Web.Bitly.Types


type UserLinkAPI = "v3" :> "user" :> "link_lookup" 
                                  :> QueryParam "access_token" Token
                                  :> QueryParam "url" UrlText 
                                  :> QueryParam "link" UrlText 
                                  :> Get '[JSON] UserLinkResponse

data UserLinkResponse = UserLinkResponse {
    links :: [UserLink]
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON UserLinkResponse where
    parseJSON (Object o) = do
      d <- o .: "data"
      UserLinkResponse <$> d .: "link_lookup"

data UserLink = UserLink {
    aggregate_link :: Maybe UrlText
  , link           :: Maybe UrlText
  , url            :: Maybe UrlText
  } deriving (Show, Eq, Generic, Ord)

instance FromJSON UserLink
