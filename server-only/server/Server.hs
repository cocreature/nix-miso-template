{-# LANGUAGE DataKinds #-}
{-# LANGUAGe DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Monoid
import Data.Time.LocalTime
import GHC.Generics
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Applicative
import Servant

data Opts = Opts { optPort :: Int, optStaticDir :: FilePath }

optParser :: Parser Opts
optParser = Opts <$> option auto (short 'p' <> value 8080) <*> strOption (short 'd' <> value "static")

main :: IO ()
main = do
  opts <- execParser (info (optParser <**> helper) fullDesc)
  let port = optPort opts
  putStrLn ("Running server on port " <> show port)
  run port (logStdoutDev (app (optStaticDir opts)))

type API = ("static" :> Raw) :<|> GetTimeAPI

type GetTimeAPI = "api" :> "time" :> Get '[JSON] Time

newtype Time = Time ZonedTime deriving Generic

instance FromJSON Time

instance ToJSON Time

app :: FilePath -> Application
app staticDir = serve (Proxy @API) (staticHandler staticDir :<|> getTimeHandler)

staticHandler :: FilePath -> Tagged Handler Application
staticHandler dir = serveDirectoryWebApp dir

getTimeHandler :: Handler Time
getTimeHandler = Time <$> liftIO getZonedTime
