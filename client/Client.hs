{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Data.Proxy
import Miso
import Servant.API

import Shared

main :: IO ()
main = do
  miso $ \currentURI ->
    App
      { model = Model currentURI
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
update' (HandleURI u) m = m { modelURI = u} <# pure NoOp
update' (ChangeURI u) m = m <# (pushURI u *> pure NoOp)
update' NoOp m = noEff m

views :: (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action)
views = viewHome :<|> viewFirst :<|> viewSecond
