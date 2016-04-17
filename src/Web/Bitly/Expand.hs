{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Web.Bitly.Expand
       (
         ExpandAPI
       ) where

import Control.Applicative
import Control.Monad.Trans.Either
import Data.Aeson (parseJSON, FromJSON, ToJSON, (.:))
import Data.Aeson.Types
import Data.Proxy
import Data.Text (Text, pack)
import GHC.Generics
import Servant.API
import Servant.Client

import Web.Bitly.Types


type ExpandAPI = "v3" :> "expand" :> QueryParam "access_token" Token
                          :> QueryParam "shortURL" UrlText 
                          :> QueryParam "hash" HashText 
                          :> Get '[JSON] ExpandResponse

data ExpandResponse = ExpandResponse {
    expanded :: [Expand]
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON ExpandResponse where
    parseJSON (Object o) = do
      d <- o .: "data"
      ExpandResponse <$> d .: "expand"

data Expand = Expand {
    global_hash :: Maybe HashText
  , user_hash   :: Maybe HashText
  , long_url    :: Maybe UrlText
  , short_url   :: Maybe UrlText
  , error       :: Maybe Error
  } deriving (Show, Eq, Generic, Ord)

instance FromJSON Expand
