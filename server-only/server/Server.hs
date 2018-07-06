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

main :: IO ()
main = putStrLn "Hello World"
