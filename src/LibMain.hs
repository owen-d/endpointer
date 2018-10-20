{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import qualified Endpoint                as Endpoint
import qualified Network.HTTP.Client.TLS as TLS
import           Network.HTTP.Simple     as Simple

main :: IO ()
main = do
  manager <- TLS.newTlsManager
  TLS.setGlobalManager manager
  -- res <- Simple.httpLBS "https://status.github.com"
  -- res <- Simple.httpLBS "https://api.github.com/users"
  -- print $ Simple.getResponseStatus res
  (Endpoint.parseStatus . Simple.getResponseStatus) <$>
    Simple.httpLBS "https://api.github.com/users"
  >>= print
  -- <*> Endpoint.parseStatus
  -- >>= print
