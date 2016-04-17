{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Web.Bitly.Lookup
       (
         LookupAPI
       ) where

import Control.Applicative
import Data.Aeson.Types
import GHC.Generics
import Servant.API

import Web.Bitly.Types

type LookupAPI = "v3" :> "link" :> "lookup" :> QueryParam "access_token" Token
                          :> QueryParam "url" UrlText 
                          :> Get '[JSON] LookupResponse

data LookupResponse = LookupResponse {
    links :: [Link]
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON LookupResponse where
    parseJSON (Object o) = do
      d <- o .: "data"
      LookupResponse <$> d .: "link_lookup"

data Link = Link {
    url             :: Maybe UrlText
  , aggregate_link  :: Maybe UrlText
  } deriving (Show, Eq, Generic, Ord)

instance FromJSON Link
