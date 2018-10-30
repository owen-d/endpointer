{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import           Control.Concurrent.Async (race, wait, waitCatch, withAsync)
import           Data.Hashable            (Hashable)
import qualified Data.UnixTime            as Time
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
  let enqueue = do
        now <- Time.getUnixTime
        let scheduleTime = Time.addUnixDiffTime now $ Time.secondsToUnixDiffTime 10
        withAsync (TQ.push tq next scheduleTime) wait
  withAsync (checkEndpoint next) $ \e -> do
    waitCatch e >>= \x -> case x of
      Left err -> print err
      Right e  -> print e
    enqueue
  loop tq

checkEndpoint :: Endpoint.Endpoint -> IO (Endpoint.EndpointStatus)
checkEndpoint endpoint = do
  req <- Simple.parseRequest endpoint
  ((Endpoint.EndpointStatus endpoint) .
   Endpoint.parseStatus . Simple.getResponseStatus) <$>
    Simple.httpLBS req
