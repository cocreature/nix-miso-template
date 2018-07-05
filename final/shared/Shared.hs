{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Shared
  ( Model(..)
  , initialModel
  , Action(..)
  , ClientRoutes
  , GetTimeAPI
  , Time(..)
  , viewHome
  , viewTime
  , getURI
  ) where

import Data.Aeson
import Data.Maybe
import Data.Proxy
import Data.Time.LocalTime
import GHC.Generics
import Miso
import Miso.String
import Servant.API
import Servant.Utils.Links

type GetTimeAPI = "api" :> "time" :> Get '[JSON] Time

newtype Time = Time ZonedTime deriving Generic

instance ToJSON Time

instance FromJSON Time

data Model = Model
  { modelURI :: URI
  , modelTime :: Maybe MisoString
  } deriving (Show, Eq)

initialModel :: URI -> Model
initialModel u = Model {modelURI = u, modelTime = Nothing}

data Action
  = NoOp
  | ChangeURI URI
  | HandleURI URI
  | SetTime ZonedTime
  | SetTimeErr MisoString
  | RefreshTime
  deriving Show

viewHome :: Model -> View Action
viewHome _ =
  div_
    []
    [ h1_ [] [text "Home"]
    , button_ [onClick (ChangeURI (getURI @("time" :> View Action)))] ["View Time"]
    ]

viewTime :: Model -> View Action
viewTime m =
  div_
    []
    [ h1_ [] [text "Current Time"]
    , div_ [] [text (fromMaybe "Time not available" (modelTime m))]
    , button_ [onClick RefreshTime] ["Refresh Time"]
    , button_ [onClick (ChangeURI (getURI @(View Action)))] ["Go Home"]
    ]

type ClientRoutes
   = View Action :<|> ("time" :> View Action)

getURI :: forall a. (HasLink a, IsElem a ClientRoutes, MkLink a ~ Link) => URI
getURI = linkURI (safeLink (Proxy :: Proxy ClientRoutes) (Proxy :: Proxy a))
