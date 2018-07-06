{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)
import Data.Time.LocalTime
import Lucid
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
    <*> strOption (short 'd' <> metavar "DIR" <> value "static")

main :: IO ()
main = do
  opts <- execParser (info (optParser <**> helper) fullDesc)
  let port = optPort opts
  putStrLn ("Starting server on port " <> show port)
  run port $ logStdoutDev (app (optStaticDir opts))

type API = "static" :> Raw :<|> GetTimeAPI :<|> ServerRoutes

type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action

newtype Wrapper a = Wrapper a

instance ToHtml a => ToHtml (Wrapper a) where
  toHtml (Wrapper a) =
    doctypehtml_ $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        script_ [src_ "static/all.js"] ("" :: Text)
      body_ $ do
        toHtml a
  toHtmlRaw = toHtml

app :: FilePath -> Application
app staticDir = serve (Proxy @API) (staticHandler staticDir :<|> getTimeHandler :<|> serverHandlers)

staticHandler :: FilePath -> Tagged Handler Application
staticHandler staticDir = serveDirectoryWebApp staticDir

getTimeHandler :: Handler Time
getTimeHandler = Time <$> liftIO getZonedTime

serverHandlers :: Handler (Wrapper (View Action)) :<|> Handler (Wrapper (View Action))
serverHandlers = homeHandler :<|> timeHandler
  where homeHandler = send viewHome (getURI @(View Action))
        timeHandler = send viewTime (getURI @("time" :> View Action))
        send f u = pure (Wrapper (f (initialModel u)))
