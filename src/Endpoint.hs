{-# LANGUAGE OverloadedStrings #-}

module Endpoint where

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C8
import           Data.Maybe                (isJust)
import qualified Network.HTTP.Simple       as Simple
import qualified Network.HTTP.Types.Status as HTStat

type Endpoint = BS.ByteString


-- will eventually want to be able to map over json returns for different endpoints
-- however, this isn't necessary initially
data HealthCheck = HttpCheck | JSONCheck

data Status = Up | Down | Unknown
  deriving (Show, Read, Eq)

parseStatus :: HTStat.Status -> Status
parseStatus (HTStat.Status code _)
  | code >= 200 && code < 300 = Up
  | otherwise = Down


-- type for union of endpoint + status. clever... i know.

data EndpointStatus = EndpointStatus
  { getEndpoint :: Endpoint
  , getStatus   :: Status
  }
  deriving (Show, Read, Eq)

isEndpoint :: Endpoint -> Bool
isEndpoint e = isJust (Simple.parseRequest $ C8.unpack e )
