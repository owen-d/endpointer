{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Postgres.Postgres where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import           Data.Maybe                 (isJust)
import           Data.Maybe                 (catMaybes)
import           Database.PostgreSQL.Simple as PG
import           Endpoint                   (Endpoint)
import qualified Endpoint                   as End
import           Text.Read                  (readMaybe)

fetchEndpoints :: PG.Connection -> IO [Endpoint]
fetchEndpoints conn = do
  let mapEndpoint (proto, e) =
        let unpacked = C8.unpack proto
         in readMaybe unpacked >>= \proto -> Just $ End.Endpoint proto e
  xs :: [(BS.ByteString, BS.ByteString)] <-
    PG.query conn "select proto, relative_url from endpoints" ()
  return . (filter End.isEndpoint) . catMaybes . (map mapEndpoint) $ xs

putEndpoint :: PG.Connection -> Endpoint -> IO ()
putEndpoint conn e = do
  PG.execute
    conn
    "insert into endpoints (proto, relative_url) values (?, ?)"
    (show e, End.relativeUrl e)
  pure ()
