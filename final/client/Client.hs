{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

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
      , subs = [uriSub HandleURI]
      , mountPoint = Nothing
      }

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
