{-# LANGUAGE OverloadedStrings #-}

module Coordinator where

import           Control.Concurrent       (Chan, MVar, forkIO, threadDelay)
import qualified Control.Concurrent       as C
import qualified Control.Concurrent.Async as Async
import           Control.Monad            (liftM, void)
import qualified Data.UnixTime            as Time
import qualified Endpoint                 as Endpoint
import qualified EndpointHeap             as EHeap
import           IOUtils                  (bindMaybe)
import qualified Network.HTTP.Client.TLS  as TLS
import qualified Network.HTTP.Simple      as Simple


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
  statusChan <- C.newChan
  Async.async $ report statusChan
  let (next, q') = EHeap.fetchNext t q
  case next of
    Nothing -> do
      threadDelay oneSecondInMicroseconds
    Just endpoint -> do
      void $ forkIO $ checkEndpoint endpoint >>= C.writeChan statusChan
  loopRounds q'

report :: Chan Endpoint.EndpointStatus -> IO ()
report ch = C.readChan ch >>= print >> report ch
