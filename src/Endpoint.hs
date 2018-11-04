{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Endpoint where

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C8
import           Data.Hashable             (Hashable)
import           Data.Maybe                (isJust)
import           GHC.Generics              (Generic)
import qualified Network.HTTP.Simple       as Simple
import qualified Network.HTTP.Simple       as Simple
import qualified Network.HTTP.Types.Status as HTStat

data Endpoint = Endpoint {proto :: Proto, relativeUrl :: BS.ByteString}
  deriving (Show, Read, Eq, Ord, Generic)
instance Hashable Endpoint

data Proto = Http | Https
  deriving (Show, Read, Eq, Ord, Generic)
instance Hashable Proto

-- will eventually want to be able to map over json returns for different endpoints
-- however, this isn't necessary initially
data HealthCheck = HttpCheck | JSONCheck

data Status = Up | Down | Unknown
  deriving (Show, Read, Eq, Ord, Generic)
instance Hashable Status

parseStatus :: HTStat.Status -> Status
parseStatus (HTStat.Status code _)
  | code >= 200 && code < 300 = Up
  | otherwise = Down


-- type for union of endpoint + status. clever... i know.

data EndpointStatus = EndpointStatus
  { getEndpoint :: Endpoint
  , getStatus   :: Status
  }
  deriving (Show, Read, Ord, Eq)

isEndpoint :: Endpoint -> Bool
isEndpoint e = isJust . Simple.parseRequest . fmtHttp $ e

fmtHttp :: Endpoint -> String
fmtHttp e =
  let prefix =
        case (proto e) of
          Http  -> "http://"
          Https -> "https://"
   in prefix ++ (C8.unpack $ relativeUrl e)


checkEndpoint :: Endpoint.Endpoint -> IO (Endpoint.EndpointStatus)
checkEndpoint endpoint = do
  req <- Simple.parseRequest $ fmtHttp endpoint
  res <- Simple.httpLBS req
  return . (EndpointStatus endpoint) . parseStatus . Simple.getResponseStatus $
    res
