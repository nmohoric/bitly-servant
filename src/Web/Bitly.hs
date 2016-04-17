{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Web.Bitly where

import Control.Monad.Trans.Either
import Data.Proxy
import Servant.API
import Servant.Client
import Web.Bitly.Types
import Web.Bitly.Expand
import Web.Bitly.Info
import Web.Bitly.Lookup
import Web.Bitly.Shorten

-- Testing only
import Data.Text (pack)

type API = ExpandAPI :<|> InfoAPI :<|> LookupAPI :<|> ShortenAPI

api :: Proxy API
api = Proxy

expand' :<|> info' :<|> linkLookup' :<|> shorten'= client api (BaseUrl Https "api-ssl.bitly.com" 443)

expand key url hash = runEitherT $ expand' (Just key) url hash
info key url hash exp = runEitherT $ info' (Just key) url hash exp
linkLookup key url = runEitherT $ linkLookup' (Just key) (Just url)
shorten key url domain = runEitherT $ shorten' (Just key) (Just url) Nothing
