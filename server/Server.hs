{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Monoid
import Data.Time.LocalTime
import Lucid
import Lucid.Base
import Miso (View, ToServerRoutes)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant

import Shared

port :: Int
port = 8080

main :: IO ()
main = do
  putStrLn ("Starting server on port " <> show port)
  run port $ logStdoutDev app

newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

instance ToHtml a => ToHtml (Wrapper a) where
  toHtmlRaw = toHtml
  toHtml (Wrapper a) =
    doctypehtml_ $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        jsRef "static/all.js"
      body_ (toHtml a)
    where
      jsRef href =
        with
          (script_ mempty)
          [ makeAttribute "src" href
          , makeAttribute "async" mempty
          , makeAttribute "defer" mempty
          ]

type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action

type API = "static" :> Raw :<|> GetTimeAPI :<|> ServerRoutes

app :: Application
app = serve (Proxy @API) (staticHandler :<|> getTimeHandler :<|> serverHandlers)

staticHandler :: Tagged Handler Application
staticHandler = serveDirectoryWebApp "static"

getTimeHandler :: Handler Time
getTimeHandler = Time <$> liftIO getZonedTime

serverHandlers :: Server ServerRoutes
serverHandlers = homeHandler :<|> timeHandler
  where
    send f u = pure (Wrapper (f (initialModel u)))
    homeHandler = send viewHome (getURI @(View Action))
    timeHandler = send viewTime (getURI @("time" :> View Action))
