{-# LANGUAGE OverloadedStrings #-}

module EndpointHeap where

import           Control.Concurrent       (Chan, MVar, forkIO, threadDelay)
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

push :: (Hashable k, Ord k, Ord p) => TaskHeap k p -> k -> p -> TaskHeap k p
push (TaskHeap q) k p = TaskHeap $ PSQ.insert k p () q

pop :: (Hashable k, Ord k, Ord p) => TaskHeap k p -> Maybe (k, TaskHeap k p)
pop (TaskHeap q) = pick <$> (PSQ.minView q)
  where pick = \(k, p, (), psq) -> (k, TaskHeap psq)
