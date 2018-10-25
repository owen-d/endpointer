{-# LANGUAGE OverloadedStrings #-}

module EndpointHeap where

import           Control.Concurrent       (Chan, MVar, forkIO, threadDelay)
import qualified Control.Concurrent       as Conc
import qualified Control.Concurrent.Async as Async
import           Data.Hashable            (Hashable)
import qualified Data.HashPSQ             as PSQ
import qualified Data.Heap                as Heap
import qualified Data.UnixTime            as Time
import qualified Endpoint                 as Endpoint

type EHeap = Heap.MinHeap EndpointSchedule

newtype EndpointSchedule = EndpointSchedule (Endpoint.Endpoint, Time.UnixTime)
  deriving (Eq, Show)

instance Ord EndpointSchedule where
  compare (EndpointSchedule(_, a)) (EndpointSchedule(_, b)) = compare a b

defaultInterval = Time.secondsToUnixDiffTime 30

unEndpointSchedule :: EndpointSchedule -> Endpoint.Endpoint
unEndpointSchedule (EndpointSchedule (e, _)) = e

mkHeap :: Time.UnixTime -> [Endpoint.Endpoint] -> EHeap
mkHeap start endpts =
  let inject = \e heap -> Heap.insert (EndpointSchedule (e, start)) heap
  in foldr inject Heap.empty endpts

-- grab looks at the next value on the heap, aborting if before currentTime.
-- Otherwise, returns the endpoint and the heap with an updated endpoint timer
check :: Time.UnixTime -> EHeap -> Maybe (Endpoint.Endpoint, EHeap)
check currentTime heap =
  let isReady (EndpointSchedule (_, a)) = currentTime > a
      increaseCheck (EndpointSchedule (a, b)) =
        EndpointSchedule (a, Time.addUnixDiffTime currentTime defaultInterval)
   in Heap.view heap >>= \(schedule, heap') ->
        if isReady schedule
          then Just
                 ( unEndpointSchedule schedule
                 , Heap.insert (increaseCheck schedule) heap')
          else Nothing

fetchNext :: Time.UnixTime -> EHeap -> (Maybe Endpoint.Endpoint, EHeap)
fetchNext t q =
  let next = check t q
  in pull q next

pull :: b -> Maybe (a,b) -> (Maybe a, b)
pull def x = let
    mapper (a,b)= (Just a, b)
  in maybe (Nothing, def) mapper x


data TaskHeap k p = TaskHeap (PSQ.HashPSQ k p ())

mkTaskHeap :: Time.UnixTime -> [Endpoint.Endpoint] -> TaskHeap Endpoint.Endpoint Time.UnixTime
mkTaskHeap start endpoints = TaskHeap . PSQ.fromList $ map (\e -> (e, start, ())) endpoints

syncPush :: (Hashable k, Ord k, Ord p) => TaskHeap k p -> k -> p -> TaskHeap k p
syncPush (TaskHeap q) k p = TaskHeap $ PSQ.insert k p () q

syncPop :: (Hashable k, Ord k, Ord p) => TaskHeap k p -> Maybe (k, TaskHeap k p)
syncPop (TaskHeap q) = pick <$> (PSQ.minView q)
  where pick = \(k, p, (), psq) -> (k, TaskHeap psq)

-- AsyncTaskHeap represents a lock for reading, a channel for writing, and an internal psq
data AsyncTaskHeap k = AsyncTaskHeap (MVar k) (Chan k) (TaskHeap k Time.UnixTime)

pop :: AsyncTaskHeap k -> IO k
pop (AsyncTaskHeap mvar _ _) = Conc.takeMVar mvar


push :: AsyncTaskHeap k -> k -> IO ()
push (AsyncTaskHeap _ chan _) = Conc.writeChan chan

-- run loops, handling 3 events: puts, pops, and ready events in queue
run :: AsyncTaskHeap k -> IO ()
run (AsyncTaskHeap _ _ psq) = do
  awaitNext psq
  where
    awaitNext psq = case (PSQ.findMin psq) of
      Nothing -> do
        _ <- Conc.newEmptyMVar
        return ()
      Just (_, ready, _) -> do
        now <- Time.getUnixTime
        let diff = now - ready
        if diff < 0 then return () else threadDelay $ unixDiffToMicroSeconds diff

unixDiffToMicroSeconds :: Time.UnixDiffTime -> Int
unixDiffToMicroSeconds (Time.UnixDiffTime seconds microseconds) =
  (seconds * (1000^2)) + microseconds
