{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Time.LocalTime
import           Lucid
import           Miso (View, ToServerRoutes)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.WebSockets
import           Options.Applicative
import           Servant
import           Servant.API.WebSocket

import           Shared

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

type API =
       "static" :> Raw
  :<|> "websocket" :> WebSocket
  :<|> GetTimeAPI
  :<|> ServerRoutes

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
app staticDir = serve (Proxy @API) (staticHandler staticDir :<|> websocketHandler :<|> getTimeHandler :<|> serverHandlers)

websocketHandler :: Connection -> Handler ()
websocketHandler conn = do
  liftIO . forM_ [1::Int ..] $ \i -> do
    sendTextData conn (Text.pack (show (show i))) >> threadDelay 1000000

serverHandlers :: Handler (Wrapper (View Action)) :<|> Handler (Wrapper (View Action))
serverHandlers = homeHandler :<|> timeHandler
  where homeHandler = pure (Wrapper (viewHome (initialModel (getURI @(View Action)))))
        timeHandler = pure (Wrapper (viewTime (initialModel (getURI @("time" :> View Action)))))

staticHandler :: FilePath -> Tagged Handler Application
staticHandler staticDir = serveDirectoryWebApp staticDir

getTimeHandler :: Maybe Text -> Handler Time
getTimeHandler p = do
  t <- liftIO getZonedTime
  pure (Time t p)
