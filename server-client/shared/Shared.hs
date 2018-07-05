{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Shared
  ( GetTimeAPI
  , Time(..)
  ) where

import Data.Aeson
import Data.Time.LocalTime
import GHC.Generics
import Servant.API

type GetTimeAPI = "api" :> "time" :> Get '[JSON] Time

newtype Time = Time ZonedTime deriving Generic

instance ToJSON Time

instance FromJSON Time
