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
import Options.Applicative
import Servant

import Shared

data Opts = Opts
  { optPort :: Int
  , optStaticDir :: FilePath
  } deriving (Show, Eq)

optParser :: Parser Opts
optParser =
  Opts
    <$> option auto (short 'p' <> metavar "PORT" <> value 8080)
    <*> strOption (short 'd' <> metavar "DOCDIR" <> value "static")

main :: IO ()
main = do
  opts <- execParser (info (optParser <**> helper) fullDesc)
  let port = optPort opts
  putStrLn ("Starting server on port " <> show port)
  run port $ logStdoutDev (app (optStaticDir opts))

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

app :: FilePath -> Application
app staticDir = serve (Proxy @API) (staticHandler staticDir :<|> getTimeHandler :<|> serverHandlers)

staticHandler :: FilePath -> Tagged Handler Application
staticHandler staticDir = serveDirectoryWebApp staticDir

getTimeHandler :: Handler Time
getTimeHandler = Time <$> liftIO getZonedTime

serverHandlers :: Server ServerRoutes
serverHandlers = homeHandler :<|> timeHandler
  where
    send f u = pure (Wrapper (f (initialModel u)))
    homeHandler = send viewHome (getURI @(View Action))
    timeHandler = send viewTime (getURI @("time" :> View Action))
