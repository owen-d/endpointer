{-# LANGUAGE OverloadedStrings #-}

module TaskQueue
  (
    TaskQueue
  , mkTaskQueue
  , push
  , pop
  )
where

import           Control.Concurrent      (Chan, MVar, forkIO, putMVar, takeMVar,
                                          threadDelay)
import qualified Control.Concurrent      as Conc
import qualified Control.Concurrent.MVar as MVar
import           Data.Hashable           (Hashable)
import qualified Data.HashPSQ            as PSQ
import qualified Data.UnixTime           as Time

defaultDelay = 1000^2

 -- refers to TaskQueue psq putMVar (pull semaphore, pull MVar)
data TaskQueue a = TaskQueue (MVar (PSQ.HashPSQ a Time.UnixTime ()))

push :: (Hashable a, Ord a) => TaskQueue a -> a -> IO ()
push (TaskQueue lock) a = do
  q <- takeMVar lock
  let q' = PSQ.insert a (Time.UnixTime 0 0) () q
  putMVar lock q'
  return ()

pop :: (Hashable a, Ord a) => TaskQueue a -> IO a
pop (TaskQueue lock) = do
  let waitUntil t = do
        now <- Time.getUnixTime
        if now > t
          then return ()
          else sleepUnix $ Time.diffUnixTime t now
  q <- takeMVar lock
  case PSQ.minView q of
    Nothing -> do
      putMVar lock q
      threadDelay defaultDelay
      pop (TaskQueue lock)
    Just (k, p, _, q') -> do
      now <- Time.getUnixTime
      if now > p
        then do
          putMVar lock q'
          return k
        else do
          putMVar lock q
          waitUntil p
          pop (TaskQueue lock)


mkTaskQueue:: (Hashable a, Ord a) => [a] -> IO (TaskQueue a)
mkTaskQueue xs = do
  start <- Time.getUnixTime
  let psq = PSQ.fromList $ map (\e -> (e, start, ())) xs
  lock <- MVar.newEmptyMVar
  putMVar lock psq
  return $ TaskQueue lock

unixToMicroSeconds :: Time.UnixDiffTime -> Int
unixToMicroSeconds (Time.UnixDiffTime seconds micro) =
  let s = (read . show $ seconds) :: Int
      micros = (read . show $ micro) :: Int
  in s * (1000^2) + micros

sleepUnix :: Time.UnixDiffTime -> IO ()
sleepUnix t = do
  threadDelay $ unixToMicroSeconds t
