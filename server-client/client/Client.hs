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
  startApp $
    App { model = initialModel currentURI
        , initialAction = NoOp
        , events = defaultEvents
        , subs = [uriSub SetURI]
        , mountPoint = Nothing
        , update = update'
        , view = \m -> case runRoute (Proxy @ClientRoutes) views modelURI m of
            Left _ -> viewHome m
            Right v -> v
        }


data Model = Model
  { modelURI :: URI
  , modelTime :: Maybe MisoString
  } deriving Eq

initialModel :: URI -> Model
initialModel uri = Model uri Nothing

data Action
  = NoOp
  | RefreshTime
  | SetTime MisoString
  | ChangeURI URI
  | SetURI URI

type ClientRoutes = View Action :<|> "time" :> View Action

getURI :: forall a. (HasLink a, IsElem a ClientRoutes, MkLink a ~ Link) => URI
getURI = linkURI (safeLink (Proxy @ClientRoutes) (Proxy @a))

views :: (Model -> View Action) :<|> (Model -> View Action)
views = viewHome :<|> viewTime

viewHome :: Model -> View Action
viewHome _ =
  div_ []
       [ h1_ [] [text "Home"]
       , button_ [onClick (ChangeURI (getURI @("time" :> View Action)))] ["View Time"]
       ]

viewTime :: Model -> View Action
viewTime m =
  div_ []
       [ h1_ [] [text "Current Time"]
       , div_ [] [text (fromMaybe "Time not available" (modelTime m))]
       , button_ [onClick RefreshTime] ["Refresh Time"]
       , button_ [onClick (ChangeURI (getURI @(View Action)))] ["Go Home"]
       ]

update' :: Action -> Model -> Effect Action Model
update' a m=
  case a of
    NoOp -> noEff m
    RefreshTime -> m <# do
      timeOrErr <- runClientM (client (Proxy @GetTimeAPI))
      pure $ case timeOrErr of
        Left err -> SetTime (ms (show err))
        Right (Time t) -> SetTime (ms (formatTime defaultTimeLocale "%H:%M:%S" t))
    SetTime t -> noEff (m { modelTime = Just t })
    ChangeURI uri -> m <# (NoOp <$ pushURI uri)
    SetURI uri -> noEff (m { modelURI = uri })
