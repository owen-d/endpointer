{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import           Control.Concurrent.Async (race, wait, withAsync)
import           Data.Hashable            (Hashable)
import qualified Endpoint                 as Endpoint
import qualified Network.HTTP.Client.TLS  as TLS
import qualified Network.HTTP.Simple      as Simple
import qualified TaskQueue                as TQ

defaultEndpoints = ["https://api.github.com/users", "https://status.github.com"]

setup :: IO ()
setup = do
  manager <- TLS.newTlsManager
  TLS.setGlobalManager manager

main :: IO ()
main = do
  setup
  tq <- TQ.mkTaskQueue defaultEndpoints
  loop tq


loop :: TQ.TaskQueue Endpoint.Endpoint -> IO ()
loop tq = do
  next <- TQ.pop tq
  withAsync (checkEndpoint next) $ \e -> wait e >>= print
  loop tq

checkEndpoint :: Endpoint.Endpoint -> IO (Endpoint.EndpointStatus)
checkEndpoint endpoint = do
  req <- Simple.parseRequest endpoint
  ((Endpoint.EndpointStatus endpoint) .
   Endpoint.parseStatus . Simple.getResponseStatus) <$>
    Simple.httpLBS req
