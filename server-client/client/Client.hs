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
import Data.Time.LocalTime
import Miso
import Miso.String
import Servant.API
import Servant.Client.Ghcjs
import Servant.Utils.Links

import Shared

main :: IO ()
main = do
  currentURI <- getCurrentURI
  startApp $
    App
      { model = initialModel currentURI
      , view = \m -> case runRoute (Proxy @ClientRoutes) views modelURI m of
          Left _ -> viewHome m
          Right v -> v
      , update = update'
      , initialAction = NoOp
      , events = defaultEvents
      , subs = [uriSub HandleURI]
      , mountPoint = Nothing
      }

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

update' :: Action -> Model -> Effect Action Model
update' NoOp m = noEff m
update' (HandleURI u) m = m { modelURI = u} <# pure NoOp
update' (ChangeURI u) m = m <# (pushURI u *> pure NoOp)
update' (SetTime t) m =
  noEff (m { modelTime = Just (ms (formatTime defaultTimeLocale "%H:%M:%S" t)) })
update' (SetTimeErr err) m = noEff (m { modelTime = Just err })
update' RefreshTime m = m <# do
  timeOrErr <- runClientM getTime
  pure $ case timeOrErr of
    Left err -> SetTimeErr (ms (show err))
    Right (Time time) -> SetTime time

views :: (Model -> View Action) :<|> (Model -> View Action)
views = viewHome :<|> viewTime

getTime :: Client ClientM GetTimeAPI
getTime = client (Proxy @GetTimeAPI)
