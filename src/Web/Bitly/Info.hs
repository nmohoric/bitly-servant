{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Web.Bitly.Info
       (
         InfoAPI
       ) where

import Control.Applicative
import Data.Aeson.Types
import GHC.Generics
import Servant.API

import Web.Bitly.Types

type InfoAPI = "v3" :> "info" :> QueryParam "access_token" Token
                          :> QueryParam "shortURL" UrlText 
                          :> QueryParam "hash" HashText 
                          :> QueryParam "expand_user" ExpandBool
                          :> Get '[JSON] InfoResponse

data InfoResponse = InfoResponse {
    info :: [Info]
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON InfoResponse where
    parseJSON (Object o) = do
      d <- o .: "data"
      InfoResponse <$> d .: "info"

data Info = Info {
    short_url   :: Maybe UrlText
  , hash        :: Maybe HashText
  , user_hash   :: Maybe HashText
  , global_hash :: Maybe HashText
  , error       :: Maybe Error
  , title       :: Maybe Title
  , created_by  :: Maybe CreatedBy
  , created_at  :: Maybe Int
  } deriving (Show, Eq, Generic, Ord)

instance FromJSON Info
