{-# LANGUAGE OverloadedStrings #-}

module Endpoint where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Types.Status  as HTStat

type Endpoint = String

-- will eventually want to be able to map over json returns for different endpoints
-- however, this isn't necessary initially
data HealthCheck = HttpCheck | JSONCheck

data Status = Up | Down
  deriving (Show, Eq)

parseStatus :: HTStat.Status -> Status
parseStatus (HTStat.Status code _)
  | code >= 200 && code < 300 = Up
  | otherwise = Down
