{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import           Config                     (Env (..), HasLog (..),
                                             HasPgConn (..), HasRedisConn (..),
                                             HasTq (..))
import qualified Config                     as Conf
import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (async, link, race, wait, waitCatch,
                                             withAsync)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import           Data.Hashable              (Hashable)
import qualified Data.Time.Clock            as Time
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis             as Red
import           Endpoint                   (checkEndpoint, getEndpoint,
                                             getStatus)
import qualified Endpoint                   as End
import qualified Network.HTTP.Client.TLS    as TLS
import qualified Postgres.Postgres          as Postgres
import           Redis                      (boundedLPush, deserializeRedis,
                                             initRedis, newEndpointTopic,
                                             scanEndpoints, subscriber)
import qualified TaskQueue                  as TQ



setup :: IO Env
setup = do
  manager <- TLS.newTlsManager
  TLS.setGlobalManager manager
  tq <- TQ.mkTaskQueue []
  pgConn <-
    PG.connect
      PG.defaultConnectInfo
        { PG.connectDatabase = Postgres.endpointsDb
        , PG.connectPassword = "secret"
        , PG.connectHost = "127.0.0.1"
        }
  redisConn <- initRedis
  return
    Env
      { envLog = putStrLn
      , envTq = tq
      , envPgConn = pgConn
      , envRedisConn = redisConn
      }



main :: IO ()
main = do
  env <- setup
  popFromRedis env env
  -- link redis receiver to main thread
  async (receiveNewEndpoints env env) >>= link
  putStrLn "looping"
  loop env $ \e -> do
    status <- checkEndpoint e
    boundedLPush env (getEndpoint status) [(getStatus status)] 100
    print status

loop :: (HasTq a) => a -> (End.Endpoint -> IO ()) -> IO ()
loop a fn = do
  let tq = (getTq a)
  next <- TQ.pop tq
  let enqueue = do
        now <- Time.getCurrentTime
        let scheduleTime =
              Time.addUTCTime (TQ.secondsToNominalDiffTime 10) now
        withAsync (TQ.push tq next scheduleTime) wait
  withAsync (fn next) wait
  enqueue
  loop tq fn

popFromRedis :: (HasRedisConn a, HasTq b) => a -> b -> IO ()
popFromRedis a b = do
  now <- Time.getCurrentTime
  let queueEndpoint (End.EndpointStatus endpoint _) = TQ.push (getTq b) endpoint now
  endpts <- scanEndpoints (getRedisConn a)
  mapM_ queueEndpoint endpts

receiveNewEndpoints :: (HasRedisConn a, HasTq b) => a -> b -> IO ()
receiveNewEndpoints a b =
  subscriber (getRedisConn a) [(C8.pack newEndpointTopic)] cb
  where
    cb msg = do
      print msg
      now <- Time.getCurrentTime
      let msg' = Red.msgMessage msg
          endpt = (deserializeRedis msg') :: End.Endpoint
      if End.isEndpoint endpt then
        TQ.push (getTq b) endpt now
      else
        putStrLn $ "invalid endpoint: " ++ (show msg')
      mempty
