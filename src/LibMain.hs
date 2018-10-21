{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import qualified Data.UnixTime           as Time
import qualified Endpoint                as Endpoint
import qualified EndpointHeap            as EHeap
import           IOUtils                 (bindMaybe)
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Simple     as Simple

defaultEndpoints = ["https://api.github.com/users", "https://status.github.com"]

checkEndpoint :: Endpoint.Endpoint -> IO (Endpoint.EndpointStatus)
checkEndpoint endpoint = do
  req <- Simple.parseRequest endpoint
  ((Endpoint.EndpointStatus endpoint) .
   Endpoint.parseStatus . Simple.getResponseStatus) <$>
    Simple.httpLBS req

setup :: IO ()
setup = do
  manager <- TLS.newTlsManager
  TLS.setGlobalManager manager

doRound :: EHeap.EHeap -> IO (Maybe Endpoint.EndpointStatus, EHeap.EHeap)
doRound q = do
  now <- Time.getUnixTime
  let (endpoint, q') = EHeap.fetchNext q now
  (flip (,) q') <$>
    bindMaybe endpoint checkEndpoint

tst = do
  now <- Time.getUnixTime
  let start = Time.UnixTime 0 0
      q = EHeap.mkHeap start defaultEndpoints
      out = EHeap.check now q
      out' = (\(a, _) -> a) <$> out
  putStrLn (show out')

main :: IO ()
main = do
  setup
  let start = Time.UnixTime 0 0
      q = EHeap.mkHeap start defaultEndpoints
  doRound q >>= \(e, _) -> putStrLn (show e)
