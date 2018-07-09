{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Data.Maybe
import Data.Proxy
import Data.Time.Format
import Miso
import Miso.String
import Servant.API
import Servant.Client.Ghcjs
import Servant.Utils.Links

import Shared

main :: IO ()
main = do
  currentURI <- getCurrentURI
  startApp $ App
    { model = initialModel currentURI
    , update = update'
    , view = \m -> case runRoute (Proxy @ClientRoutes) views modelURI m of
        Left _ -> viewHome m
        Right v -> v
    , initialAction = NoOp
    , subs = [ uriSub SetURI ]
    , events = defaultEvents
    , mountPoint = Nothing
    }

data Model = Model { modelURI :: URI, modelTime :: Maybe MisoString } deriving Eq

initialModel :: URI -> Model
initialModel u = Model u Nothing

data Action
  = NoOp
  | SetURI URI
  | ChangeURI URI
  | RefreshTime
  | SetTime MisoString

type ClientRoutes = View Action :<|> "time" :> View Action

views :: (Model -> View Action) :<|> (Model -> View Action)
views = viewHome :<|> viewTime

viewHome :: Model -> View Action
viewHome _ =
  div_
    []
    [ h1_ [] [text "Home"]
    , button_ [onClick (ChangeURI (getURI @("time" :> View Action)))] ["View time"]
    ]

viewTime :: Model -> View Action
viewTime m =
  div_
    []
    [ h1_ [] [text "Current Time"]
    , button_ [onClick RefreshTime] [text "Refresh Time"]
    , div_ [] [text (fromMaybe "Time unavailable" (modelTime m))]
    , button_ [onClick (ChangeURI (getURI @(View Action)))] [text "Go back home"]
    ]

getURI :: forall a. (HasLink a, IsElem a ClientRoutes, MkLink a ~ Link) => URI
getURI = linkURI (safeLink (Proxy @ClientRoutes) (Proxy @a))

update' :: Action -> Model -> Effect Action Model
update' a m =
  case a of
    NoOp -> noEff m
    SetURI u -> noEff (m { modelURI = u })
    SetTime t -> noEff (m { modelTime = Just t })
    ChangeURI u -> m <# (pushURI u >> pure NoOp)
    RefreshTime -> m <# do
      let getTime = client (Proxy @GetTimeAPI)
      res <- runClientM getTime
      case res of
        Left err -> pure (SetTime (ms (show err)))
        Right (Time t) -> pure (SetTime (ms (formatTime defaultTimeLocale "%H:%M:%S" t)))
