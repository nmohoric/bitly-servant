{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Web.Bitly.Shorten
       (
         ShortenAPI
       ) where

import Control.Applicative 
import Data.Aeson.Types
import GHC.Generics
import Servant.API

import Web.Bitly.Types

type ShortenAPI = "v3" :> "shorten" :> QueryParam "access_token" Token
                          :> QueryParam "longUrl" UrlText 
                          :> QueryParam "domain" Domain
                          :> Get '[JSON] ShortenResponse

data ShortenResponse = ShortenResponse {
    global_hash :: Maybe HashText
  , hash        :: Maybe HashText
  , long_url    :: Maybe UrlText
  , new_hash    :: Maybe Int
  , url         :: Maybe UrlText
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON ShortenResponse where
  parseJSON (Object o) = 
        ShortenResponse <$>
         (d >>= (.:? "global_hash")) <*>
         (d >>= (.:? "hash")) <*>
         (d >>= (.:? "long_url")) <*>
         (d >>= (.:? "new_hash")) <*>
         (d >>= (.:? "url"))
         where d = (o .: "data")
