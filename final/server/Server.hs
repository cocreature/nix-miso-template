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

type API = "static" :> Raw :<|> GetTimeAPI

app :: FilePath -> Application
app staticDir = serve (Proxy @API) (staticHandler staticDir :<|> getTimeHandler)

staticHandler :: FilePath -> Tagged Handler Application
staticHandler staticDir = serveDirectoryWebApp staticDir

getTimeHandler :: Handler Time
getTimeHandler = Time <$> liftIO getZonedTime
