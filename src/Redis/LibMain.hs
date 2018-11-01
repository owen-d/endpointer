{-# LANGUAGE OverloadedStrings #-}

module Redis.LibMain where

import           Endpoint
import           Redis

defaultEndpoints = ["https://api.github.com/users", "https://status.github.com"]

main :: IO ()
main = do
  conn <- initRedis
  let defaultCacher e = cacheEndpoint conn $ EndpointStatus e Unknown
  mapM_ defaultCacher defaultEndpoints
  found <- scanEndpoints conn
  print found


