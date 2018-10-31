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

init :: IO Red.Connection
init = connect defaultConnectInfo

cacheEndpoint :: Red.Connection -> EndpointStatus -> IO ()
cacheEndpoint conn (EndpointStatus endpt status) = do
  runRedis conn $
    void $ setex endpt hrInSeconds (C8.pack . show $ status)

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
