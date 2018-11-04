{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Database.PostgreSQL.Simple as PG
import qualified Database.Redis             as Red
import qualified Endpoint                   as End
import qualified TaskQueue                  as TQ

-- Config setup
data Env = Env
  { envLog       :: (String -> IO ())
  , envTq        :: TQ.TaskQueue End.Endpoint
  , envPgConn    :: PG.Connection
  , envRedisConn :: Red.Connection
  }

class HasLog a where
  getLog :: a -> (String -> IO ())
instance HasLog (String -> IO ()) where
  getLog = id
instance HasLog Env where
  getLog = envLog

class HasTq a where
  getTq :: a -> TQ.TaskQueue End.Endpoint
instance HasTq (TQ.TaskQueue End.Endpoint) where
  getTq = id
instance HasTq Env where
  getTq = envTq

class HasPgConn a where
  getPgConn :: a -> PG.Connection
instance HasPgConn PG.Connection where
  getPgConn = id
instance HasPgConn Env where
  getPgConn = envPgConn

class HasRedisConn a where
  getRedisConn :: a -> Red.Connection
instance HasRedisConn Red.Connection where
  getRedisConn = id
instance HasRedisConn Env where
  getRedisConn = envRedisConn

