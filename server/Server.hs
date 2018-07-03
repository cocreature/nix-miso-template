{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Lucid
import Lucid.Base
import Miso (View, ToServerRoutes(..))
import Network.Wai.Handler.Warp
import Servant

import Example

port :: Int
port = 8080

main :: IO ()
main = do
  run port $ app

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

type API = "static" :> Raw :<|> ServerRoutes

app :: Application
app = serve (Proxy @API) (staticHandler :<|> serverHandlers)

staticHandler = serveDirectoryWebApp "static"

serverHandlers :: Server ServerRoutes
serverHandlers = homeHandler :<|> firstHandler :<|> secondHandler
  where
    send f u = pure (Wrapper (f (Model {modelURI = u})))
    homeHandler = send viewHome (getURI @(View Action))
    firstHandler = send viewFirst (getURI @("first" :> View Action))
    secondHandler = send viewSecond (getURI @("second" :> View Action))
