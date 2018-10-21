{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import qualified Data.Time               as Time
import qualified Endpoint                as Endpoint
import qualified EndpointHeap            as EHeap
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Simple     as Simple

checkEndpoint :: Endpoint.Endpoint -> IO (Endpoint.Status)
checkEndpoint endpoint = do
  req <- Simple.parseRequest endpoint
  (Endpoint.parseStatus . Simple.getResponseStatus) <$> Simple.httpLBS req

fetchNext :: EHeap.EHeap -> Time.UTCTime -> (Maybe Endpoint.Endpoint, EHeap.EHeap)
fetchNext q t =
  let next = EHeap.check t q
  in pull q next

pull :: b -> Maybe (a,b) -> (Maybe a, b)
pull def x = let
    mapper (a,b)= (Just a, b)
  in maybe (Nothing, def) mapper x

setup :: IO ()
setup = do
  manager <- TLS.newTlsManager
  TLS.setGlobalManager manager

defaultEndpoints = ["https://api.github.com/users", "https://status.github.com"]

main :: IO ()
main = do
  setup
  now <- Time.getCurrentTime
  let start = fromInteger 0 :: Time.UTCTime
      q = EHeap.mkHeap start defaultEndpoints
  return ()

