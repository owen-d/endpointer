{-# LANGUAGE OverloadedStrings #-}

module Redis where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as C8
import           Database.Redis         (connect, defaultConnectInfo, get,
                                         runRedis, set, setex)
import qualified Database.Redis         as Red
import           Endpoint               (Endpoint, EndpointStatus (..),
                                         Status (..))


hrInSeconds = 60^2

initRedis :: IO Red.Connection
initRedis = connect defaultConnectInfo

class (Show a) => RedisSerializable a where
  redisFmt :: a -> BS.ByteString
  redisFmt = C8.pack . show

instance RedisSerializable Status
instance RedisSerializable EndpointStatus
instance RedisSerializable BS.ByteString where
  redisFmt = id

cacheEndpoint :: Red.Connection -> EndpointStatus -> IO ()
cacheEndpoint conn (EndpointStatus endpt status) = do
  cache conn endpt status hrInSeconds

cache ::
     (RedisSerializable k, RedisSerializable v)
  => Red.Connection
  -> k
  -> v
  -> Integer
  -> IO ()
cache conn k v expiry = runRedis conn $
  void $ setex (redisFmt k) expiry $ redisFmt v

scanEndpoints :: Red.Connection -> IO [EndpointStatus]
scanEndpoints conn = scanAcc conn [] Red.cursor0

scanAcc :: Red.Connection -> [EndpointStatus] -> Red.Cursor -> IO [EndpointStatus]
scanAcc conn acc cursor =
  runRedis conn $ do
    res <- Red.scan cursor
    case res of
      Left _ -> return acc
      Right (cursor', endpts)
        | cursor' == Red.cursor0 -> return $ acc ++ (mapEndpts endpts)
      Right (cursor', endpts) -> liftIO $ scanAcc conn (mapEndpts endpts) cursor'

mapEndpts :: [C8.ByteString] -> [EndpointStatus]
mapEndpts xs = map (\x -> EndpointStatus (x :: BS.ByteString) Up) xs
