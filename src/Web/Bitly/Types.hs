{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Web.Bitly.Types where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Servant.API (ToText, FromText)

newtype HashText = HashText Text
  deriving (Show, Eq, Ord, FromText, ToText, FromJSON)

newtype UrlText = UrlText Text
  deriving (Show, Eq, Ord, FromText, ToText, FromJSON)

newtype Token = Token Text
  deriving (Show, Eq, Ord, FromText, ToText)

newtype ExpandBool = ExpandBool Bool
  deriving (Show, Eq, Ord, ToText)

newtype Title = Title Text
  deriving (Show, Eq, Ord, FromText, ToText, FromJSON)

newtype CreatedBy = CreatedBy Text
  deriving (Show, Eq, Ord, FromText, ToText, FromJSON)

newtype Error = Error Text
  deriving (Show, Eq, Ord, FromText, ToText, FromJSON)
