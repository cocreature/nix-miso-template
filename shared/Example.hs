{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Example
  ( Model(..)
  , Action(..)
  , ClientRoutes
  , viewHome
  , viewFirst
  , viewSecond
  , getURI
  ) where

import Data.Proxy
import Miso
import Servant.API
import Servant.Utils.Links

data Model = Model
  { modelURI :: URI
  } deriving (Show, Eq)

data Action
  = NoOp
  | ChangeURI URI
  | HandleURI URI
  deriving (Show, Eq)

viewHome :: Model -> View Action
viewHome _ =
  div_
    []
    [ h1_ [] [text "Home"]
    , button_ [onClick (ChangeURI (getURI @("first" :> View Action)))] ["Switch to first subpage"]
    , button_ [onClick (ChangeURI (getURI @("second" :> View Action)))] ["Switch to second subpage"]
    ]

viewFirst :: Model -> View Action
viewFirst _ =
  div_
    []
    [ h1_ [] [text "First subpage"]
    , button_ [onClick (ChangeURI (getURI @(View Action)))] ["Go home"]
    ]

viewSecond :: Model -> View Action
viewSecond _ =
  div_
    []
    [ h1_ [] [text "Second subpage"]
    , button_ [onClick (ChangeURI (getURI @(View Action)))] ["Go home"]
    ]

type ClientRoutes
   = View Action :<|> "first" :> View Action :<|> "second" :> View Action

getURI :: forall a. (HasLink a, IsElem a ClientRoutes, MkLink a ~ Link) => URI
getURI = linkURI (safeLink (Proxy :: Proxy ClientRoutes) (Proxy :: Proxy a))
