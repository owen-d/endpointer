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
import qualified ReadUtils                 as RU

data Endpoint = Endpoint
  { proto       :: Proto
  , relativeUrl :: BS.ByteString
  } deriving (Eq, Ord, Generic)
instance Hashable Endpoint

instance Show Endpoint where
  show e = (show $ proto e) ++ (C8.unpack $ relativeUrl e)

instance Read Endpoint where
  readsPrec i str =
    case (readsPrec i str :: [(Proto, String)]) of
      []          -> []
      [(p, rest)] -> [(Endpoint p $ C8.pack rest, "")]

data Proto = Http | Https
  deriving (Eq, Ord, Generic)
instance Hashable Proto


instance Show Proto where
  show Https = "https://"
  show Http  = "http://"

instance Read Proto where
  readsPrec _ str =
    RU.readMatch [(Http, show Http), (Https, show Https)] str


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
isEndpoint e = isJust . Simple.parseRequest $ show e

checkEndpoint :: Endpoint.Endpoint -> IO (Endpoint.EndpointStatus)
checkEndpoint endpoint = do
  req <- Simple.parseRequest $ show endpoint
  res <- Simple.httpLBS req
  return . (EndpointStatus endpoint) . parseStatus . Simple.getResponseStatus $
    res
