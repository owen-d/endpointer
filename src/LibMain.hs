{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import           Control.Concurrent      (threadDelay)
import qualified Coordinator             as Coord
import qualified Data.UnixTime           as Time
import qualified Endpoint                as Endpoint
import qualified EndpointHeap            as EHeap
import           IOUtils                 (bindMaybe)
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Simple     as Simple

defaultEndpoints = ["https://api.github.com/users", "https://status.github.com"]

setup :: IO ()
setup = do
  manager <- TLS.newTlsManager
  TLS.setGlobalManager manager

main :: IO ()
main = do
  setup
  let start = Time.UnixTime 0 0
      q = EHeap.mkHeap start defaultEndpoints
  Coord.loopRounds q
