{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Data.Maybe
import Data.Proxy
import Data.Time.Format
import Miso
import Miso.String
import Servant.API
import Servant.Client.Ghcjs
import Servant.Utils.Links

import Shared

main :: IO ()
main = putStrLn "Hello World"
