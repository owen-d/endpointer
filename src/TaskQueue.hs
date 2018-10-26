{-# LANGUAGE OverloadedStrings #-}

module TaskQueue where

import           Control.Concurrent       (Chan, MVar, forkIO, putMVar,
                                           takeMVar, threadDelay)
import qualified Control.Concurrent       as Conc
import           Control.Concurrent.Async (async, race, wait, withAsync)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar  as MVar
import           Data.Hashable            (Hashable)
import qualified Data.HashPSQ             as PSQ
import qualified Data.UnixTime            as Time

-- refers to TaskQueue psq putMVar (pull semaphore, pull MVar)
data TaskQueue a = TaskQueue (PSQ.HashPSQ a Time.UnixTime ()) (MVar a) (MVar (), MVar a)

mkTaskQueue:: (Hashable a, Ord a) => [a] -> IO (TaskQueue a)
mkTaskQueue xs = do
  start <- Time.getUnixTime
  let psq = PSQ.fromList $ map (\e -> (e, start, ())) xs
  pushM <- MVar.newEmptyMVar
  pullM <- MVar.newEmptyMVar
  sem <- MVar.newEmptyMVar
  return $ TaskQueue psq pushM (sem, pullM)

empty :: TaskQueue a -> Bool
empty (TaskQueue q _ _) = PSQ.null q

pop :: TaskQueue a -> IO a
pop (TaskQueue _ _ (sem, mvar)) = withAsync (takeMVar sem) $ \_ -> do
  res <- async $ takeMVar mvar
  wait res

push :: TaskQueue a -> a -> IO ()
push (TaskQueue _ mvar _) a = async (putMVar mvar a) >>= wait

syncPush :: (Hashable a, Ord a) => TaskQueue a -> a -> Time.UnixTime -> TaskQueue a
syncPush (TaskQueue psq a b) key time = TaskQueue (PSQ.insert key time () psq) a b

syncPop :: (Hashable a, Ord a) => TaskQueue a -> Maybe (a, TaskQueue a)
syncPop (TaskQueue q a b) = pick <$> (PSQ.minView q)
  where pick = \(k, p, (), psq) -> (k, TaskQueue psq a b)

unixToMicroSeconds :: Time.UnixDiffTime -> Int
unixToMicroSeconds (Time.UnixDiffTime seconds micro) =
  let s = (read . show $ seconds) :: Int
      micros = (read . show $ micro) :: Int
  in s * (1000^2) + micros

sleepUnix :: Time.UnixDiffTime -> IO ()
sleepUnix t = do
  threadDelay $ unixToMicroSeconds t

-- returns an Int so we can use maxBound
-- and can be consumed by threadDelay
timeUntilNext :: (Hashable a, Ord a) => TaskQueue a -> IO (Maybe Int)
timeUntilNext (TaskQueue q _ _) =
  if PSQ.null q
    then (return . Just) maxBound
    else do
      t <- Time.getUnixTime
      let result = PSQ.findMin q >>= (mapper t)
      return result
  where
    mapper t =
      \(_, p, _) ->
        let diff = Time.diffUnixTime p t
         in if diff > 0
              then Just $ unixToMicroSeconds diff
              else Nothing

run :: (Hashable a, Ord a) => TaskQueue a -> IO ()
run q = do
  let waitOnPush = undefined
      waitOnPull = undefined
      append new = do
        now <- Time.getUnixTime
        run $ syncPush q new now
      delayUntil = undefined
  first <- race waitOnPush waitOnPull
  case first of
    Left new -> append new
    Right _ -> do
      let isEmpty = empty q
      if isEmpty
        then waitOnPush >>= append
        else syncPop q >>= \(k, q) -> return ()


