{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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

import Shared

main :: IO ()
main = do
  miso $ \currentURI ->
    App
      { model = initialModel currentURI
      , view = \m -> case runRoute (Proxy @ClientRoutes) views modelURI m of
          Left _ -> viewHome m
          Right v -> v
      , update = update'
      , initialAction = NoOp
      , events = defaultEvents
      , subs =
          [ uriSub HandleURI
          , websocketSub
              (URL "ws://localhost:8080/websocket")
              (Protocols [])
              HandleWebSocket
          ]
      , mountPoint = Nothing
      }

update' :: Action -> Model -> Effect Action Model
update' NoOp m = noEff m
update' (HandleURI u) m = noEff (m { modelURI = u})
update' (ChangeURI u) m = m <# (pushURI u *> pure NoOp)
update' (SetTime t) m =
  noEff (m { modelTime = Just t })
update' RefreshTime m = m <# do
  timeOrErr <- runClientM (getTime (Just "foobar"))
  pure $ case timeOrErr of
    Left err -> SetTime (ms (show err))
    Right (Time time param) ->
      SetTime (ms (formatTime defaultTimeLocale "%H:%M:%S" time) <> fromMaybe "" param)
update' (HandleWebSocket (WebSocketMessage (Message msg))) m =
  noEff (m { modelLastMessage = Just msg })
update' (HandleWebSocket (WebSocketClose {})) m = m <# (NoOp <$ putStrLn "web socket closed")
update' (HandleWebSocket WebSocketOpen) m = m <# (NoOp <$ putStrLn "web socket open")
update' (HandleWebSocket (WebSocketError {})) m = m <# (NoOp <$ putStrLn "web socket error")

views :: (Model -> View Action) :<|> (Model -> View Action)
views = viewHome :<|> viewTime


getTime :: Client ClientM GetTimeAPI
getTime = client (Proxy @GetTimeAPI)
