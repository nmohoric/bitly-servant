{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Web.Bitly where

import Control.Monad.Trans.Either
import Data.Proxy
import Servant.Client
import Web.Bitly.Expand

type API = ExpandAPI

api :: Proxy API
api = Proxy

getExpanded = client api (BaseUrl Https "api-ssl.bitly.com" 443)

expand :: Token
       -> Maybe UrlText
       -> Maybe HashText
       -> IO (Either ServantError ExpandResponse)
expand key url hash = runEitherT $ getExpanded (Just key) url hash
