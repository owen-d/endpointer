{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, link, race, wait, waitCatch,
                                           withAsync)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C8
import           Data.Hashable            (Hashable)
import qualified Data.Time.Clock          as Time
import qualified Database.Redis           as Red
import           Endpoint                 (checkEndpoint, getEndpoint,
                                           getStatus)
import qualified Endpoint                 as Endpoint
import qualified Network.HTTP.Client.TLS  as TLS
import           Redis                    (boundedLPush, deserializeRedis,
                                           initRedis, newEndpointTopic,
                                           scanEndpoints, subscriber)
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
  loop tq $ \e -> do
    status <- checkEndpoint e
    boundedLPush redisConn (getEndpoint status) [(getStatus status)] 100
    print status

loop :: TQ.TaskQueue Endpoint.Endpoint -> (Endpoint.Endpoint -> IO ()) -> IO ()
loop tq fn = do
  next <- TQ.pop tq
  let enqueue = do
        now <- Time.getCurrentTime
        let scheduleTime =
              Time.addUTCTime (TQ.secondsToNominalDiffTime 10) now
        withAsync (TQ.push tq next scheduleTime) wait
  withAsync (fn next) wait
  enqueue
  loop tq fn

popFromRedis :: Red.Connection -> TQ.TaskQueue Endpoint.Endpoint -> IO ()
popFromRedis conn tq = do
  now <- Time.getCurrentTime
  let queueEndpoint (Endpoint.EndpointStatus endpoint _) = TQ.push tq endpoint now
  endpts <- scanEndpoints conn
  mapM_ queueEndpoint endpts

receiveNewEndpoints :: Red.Connection -> TQ.TaskQueue Endpoint.Endpoint -> IO ()
receiveNewEndpoints conn tq =
  subscriber conn [(C8.pack newEndpointTopic)] cb
  where
    cb msg = do
      print msg
      now <- Time.getCurrentTime
      let msg' = Red.msgMessage msg
          endpt = (deserializeRedis msg') :: Endpoint.Endpoint
      if Endpoint.isEndpoint endpt then
        TQ.push tq endpt now
      else
        putStrLn $ "invalid endpoint: " ++ (show msg')
      mempty
