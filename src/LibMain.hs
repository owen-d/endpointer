{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, link, race, wait, waitCatch,
                                           withAsync)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C8
import           Data.Hashable            (Hashable)
import qualified Data.UnixTime            as Time
import qualified Database.Redis           as Red
import qualified Endpoint                 as Endpoint
import qualified Network.HTTP.Client.TLS  as TLS
import qualified Network.HTTP.Simple      as Simple
import           Redis                    (cacheEndpoint, initRedis,
                                           newEndpointTopic, scanEndpoints,
                                           subscriber)
import qualified TaskQueue                as TQ


setup :: IO ()
setup = do
  manager <- TLS.newTlsManager
  TLS.setGlobalManager manager

main :: IO ()
main = do
  setup
  tq <- TQ.mkTaskQueue []
  redisConn <- initRedis
  popFromRedis redisConn tq
  -- link redis receiver to main thread
  async (receiveNewEndpoints redisConn tq) >>= link
  putStrLn "looping"
  loop tq


loop :: TQ.TaskQueue Endpoint.Endpoint -> IO ()
loop tq = do
  next <- TQ.pop tq
  let enqueue = do
        now <- Time.getUnixTime
        let scheduleTime =
              Time.addUnixDiffTime now $ Time.secondsToUnixDiffTime 10
        withAsync (TQ.push tq next scheduleTime) wait
  withAsync (checkEndpoint next) $ \e -> do
    waitCatch e >>= \x ->
      case x of
        Left err -> print err
        Right e  -> print e
    enqueue
  loop tq

checkEndpoint :: Endpoint.Endpoint -> IO (Endpoint.EndpointStatus)
checkEndpoint endpoint = do
  req <- Simple.parseRequest (C8.unpack endpoint)
  ((Endpoint.EndpointStatus endpoint) .
   Endpoint.parseStatus . Simple.getResponseStatus) <$>
    Simple.httpLBS req

popFromRedis :: Red.Connection -> TQ.TaskQueue Endpoint.Endpoint -> IO ()
popFromRedis conn tq = do
  let queueEndpoint (Endpoint.EndpointStatus endpoint _) = TQ.push tq endpoint (Time.UnixTime 0 0)
  endpts <- scanEndpoints conn
  mapM_ queueEndpoint endpts

receiveNewEndpoints :: Red.Connection -> TQ.TaskQueue Endpoint.Endpoint -> IO ()
receiveNewEndpoints conn tq =
  subscriber conn [(C8.pack newEndpointTopic)] cb
  where
    cb msg = do
      print msg
      TQ.push tq (Red.msgMessage msg) $ Time.UnixTime 0 0
      putStrLn "queued"
      mempty
