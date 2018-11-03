{-# LANGUAGE OverloadedStrings #-}

module TaskQueue
  (
    TaskQueue
  , mkTaskQueue
  , push
  , pop
  , secondsToNominalDiffTime
  )
where

import           Control.Concurrent      (Chan, MVar, forkIO, putMVar, takeMVar,
                                          threadDelay)
import qualified Control.Concurrent      as Conc
import qualified Control.Concurrent.MVar as MVar
import           Data.Hashable           (Hashable)
import qualified Data.HashPSQ            as PSQ
import qualified Data.Time.Clock         as Time

secondsToMicroSeconds :: (Num a, RealFrac a) => a -> Int
secondsToMicroSeconds x = floor $ x * (1000^2)

secondsToNominalDiffTime :: Integer -> Time.NominalDiffTime
secondsToNominalDiffTime x = fromInteger x :: Time.NominalDiffTime

defaultDelay = secondsToMicroSeconds 1

 -- refers to TaskQueue psq putMVar (pull semaphore, pull MVar)
data TaskQueue a = TaskQueue (MVar (PSQ.HashPSQ a Time.UTCTime ()))

push :: (Hashable a, Ord a) => TaskQueue a -> a -> Time.UTCTime -> IO ()
push (TaskQueue lock) a scheduleTime = do
  q <- takeMVar lock
  let q' = PSQ.insert a scheduleTime () q
  putMVar lock q'
  return ()

pop :: (Hashable a, Ord a) => TaskQueue a -> IO a
pop (TaskQueue lock) = do
  q <- takeMVar lock
  case PSQ.minView q of
    Nothing -> do
      putMVar lock q
      threadDelay defaultDelay
      pop (TaskQueue lock)
    Just (k, p, _, q') -> do
      now <- Time.getCurrentTime
      if now > p
        then do
          putMVar lock q'
          return k
        else do
          putMVar lock q
          threadDelay $ secondsToMicroSeconds $ Time.diffUTCTime now p
          pop (TaskQueue lock)


mkTaskQueue:: (Hashable a, Ord a) => [a] -> IO (TaskQueue a)
mkTaskQueue xs = do
  start <- Time.getCurrentTime
  let psq = PSQ.fromList $ map (\e -> (e, start, ())) xs
  lock <- MVar.newEmptyMVar
  putMVar lock psq
  return $ TaskQueue lock
