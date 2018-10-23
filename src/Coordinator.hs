{-# LANGUAGE OverloadedStrings #-}

module Coordinator where

import           Control.Concurrent      (MVar, forkIO, threadDelay)
import qualified Control.Concurrent      as C
import           Control.Monad           (liftM)
import qualified Data.UnixTime           as Time
import qualified Endpoint                as Endpoint
import qualified EndpointHeap            as EHeap
import           IOUtils                 (bindMaybe)
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Simple     as Simple


-- need a way to spin up workers
-- need to wrap heap actions in MVar

oneSecondInMicroseconds = 1000^2

checkEndpoint :: Endpoint.Endpoint -> IO (Endpoint.EndpointStatus)
checkEndpoint endpoint = do
  req <- Simple.parseRequest endpoint
  ((Endpoint.EndpointStatus endpoint) .
   Endpoint.parseStatus . Simple.getResponseStatus) <$>
    Simple.httpLBS req

loopRounds :: EHeap.EHeap -> IO ()
loopRounds q = do
  t <- Time.getUnixTime
  let ready = EHeap.check t q
  case ready of
    Nothing -> do
      threadDelay oneSecondInMicroseconds
      putStrLn . show $ ready
      loopRounds q
    Just (endpoint, q') -> do
      forkIO $ checkEndpoint endpoint >>= putStrLn . show
      loopRounds q'
