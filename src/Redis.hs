{-# LANGUAGE OverloadedStrings #-}

module Redis where

import           Config                 (HasRedisConn, getRedisConn)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as C8
import           Database.Redis         (connect, defaultConnectInfo, get,
                                         runRedis, set, setex)
import qualified Database.Redis         as Red
import           Endpoint               (Endpoint, EndpointStatus (..),
                                         Status (..), isEndpoint)

newEndpointTopic = "endpoints/new"
hrInSeconds = 60^2

initRedis :: IO Red.Connection
initRedis = connect defaultConnectInfo

class (Show a, Read a) =>
      RedisSerializable a
  where
  redisFmt :: a -> BS.ByteString
  redisFmt = C8.pack . show
  deserializeRedis :: BS.ByteString -> a
  deserializeRedis = read . C8.unpack

instance RedisSerializable Endpoint
instance RedisSerializable Status
instance RedisSerializable EndpointStatus
instance RedisSerializable BS.ByteString where
  redisFmt = id

cacheEndpoint :: HasRedisConn a => a -> EndpointStatus -> IO ()
cacheEndpoint a (EndpointStatus endpt status) = do
  cache a endpt status hrInSeconds

cache ::
     (HasRedisConn a, RedisSerializable k, RedisSerializable v)
  => a
  -> k
  -> v
  -> Integer
  -> IO ()
cache a k v expiry = runRedis (getRedisConn a) $
  void $ setex (redisFmt k) expiry $ redisFmt v

scanEndpoints :: HasRedisConn a => a -> IO [EndpointStatus]
scanEndpoints a = scanAcc (getRedisConn a) [] Red.cursor0

-- scanAcc only receives keys, so we set the default statuses to Unknown. This
-- is intended to be used when initially populating the worker.
scanAcc :: HasRedisConn a => a -> [EndpointStatus] -> Red.Cursor -> IO [EndpointStatus]
scanAcc a acc cursor =
  runRedis (getRedisConn a) $ do
    res <- Red.scan cursor
    case res of
      Left _ -> return acc
      Right (cursor', endpts)
        | cursor' == Red.cursor0 -> return $ acc ++ (mapEndpts Unknown endpts)
      Right (cursor', endpts) ->
        let newEndpts = mapEndpts Unknown endpts
        in
          liftIO $ scanAcc a (acc ++ newEndpts) cursor'

mapEndpts :: Status -> [C8.ByteString] -> [EndpointStatus]
mapEndpts status xs =
  [ es
  | x <- xs
  , let e = deserializeRedis x :: Endpoint
        es = EndpointStatus e status
  , isEndpoint e
  ]

subscriber :: HasRedisConn a => a -> [BS.ByteString] -> (Red.Message -> IO Red.PubSub) -> IO ()
subscriber a chans callback =
  runRedis (getRedisConn a) $ Red.pubSub (Red.subscribe chans) callback

-- TODO: error handling
boundedLPush :: (HasRedisConn a, RedisSerializable k, RedisSerializable v) => a ->  k -> [v] -> Integer -> IO ()
boundedLPush a k vs maxLength = runRedis (getRedisConn a) $ do
  Red.lpush (redisFmt k) (map redisFmt vs)
  Red.ltrim (redisFmt k) 0 $ maxLength - 1
  pure ()
