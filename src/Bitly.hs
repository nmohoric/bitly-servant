{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Bitly where

import           Control.Applicative
import           Control.Monad.Trans.Either
import Data.Aeson (parseJSON, FromJSON, ToJSON, (.:))
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Text (Text, pack)
import GHC.Generics
import Servant.API
import Servant.Client

-- | Generic Types
newtype HashText = HashText Text
  deriving (Show, Eq, Ord, FromText, ToText, FromJSON)

newtype UrlText = UrlText Text
  deriving (Show, Eq, Ord, FromText, ToText, FromJSON)

newtype Token = Token Text
  deriving (Show, Eq, Ord, FromText, ToText)

newtype Error = Error Text
  deriving (Show, Eq, Ord, FromText, ToText, FromJSON)

-- | Expand Response
data ExpandResponse = ExpandResponse {
    expanded :: [Expand]
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON ExpandResponse where
    parseJSON (Object o) = do
      d <- o .: "data"
      ExpandResponse <$> d .: "expand"

-- | Expand
data Expand = Expand {
    global_hash :: Maybe HashText
  , user_hash   :: Maybe HashText
  , long_url    :: Maybe UrlText
  , short_url   :: Maybe UrlText
  , error       :: Maybe Error
  } deriving (Show, Eq, Generic, Ord)

instance FromJSON Expand

-- | Expand API
type ExpandAPI = "v3" :> "expand" :> QueryParam "access_token" Token
                          :> QueryParam "shortURL" UrlText 
                          :> QueryParam "hash" HashText 
                          :> Get '[JSON] ExpandResponse
type API = ExpandAPI

api :: Proxy API
api = Proxy

getExpanded = client api (BaseUrl Https "api-ssl.bitly.com" 443)

expand :: Token
       -> Maybe UrlText
       -> Maybe HashText
       -> IO (Either ServantError ExpandResponse)
expand key url hash = runEitherT $ getExpanded (Just key) url hash
