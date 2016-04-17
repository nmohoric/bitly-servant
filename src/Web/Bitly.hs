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

-- Testing only
import Data.Text (pack)

type API = ExpandAPI :<|> InfoAPI

api :: Proxy API
api = Proxy

expand' :<|> info' = client api (BaseUrl Https "api-ssl.bitly.com" 443)

expand key url hash = runEitherT $ expand' (Just key) url hash
info key url hash exp = runEitherT $ info' (Just key) url hash exp

