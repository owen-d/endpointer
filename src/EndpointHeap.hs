{-# LANGUAGE OverloadedStrings #-}

module EndpointHeap where

import qualified Data.Heap as Heap
import qualified Data.Time as Time
import qualified Endpoint  as Endpoint

type EHeap = Heap.MinHeap EndpointSchedule

newtype EndpointSchedule = EndpointSchedule (Endpoint.Endpoint, Time.UTCTime)
  deriving (Eq, Show)

instance Ord EndpointSchedule where
  compare (EndpointSchedule(_, a)) (EndpointSchedule(_, b)) = compare a b

unEndpointSchedule :: EndpointSchedule -> Endpoint.Endpoint
unEndpointSchedule (EndpointSchedule (e, _)) = e

mkHeap :: Time.UTCTime -> [Endpoint.Endpoint] -> EHeap
mkHeap start endpts =
  let inject = \e heap -> Heap.insert (EndpointSchedule (e, start)) heap
  in foldr inject Heap.empty endpts

-- grab looks at the next value on the heap, aborting if before currentTime.
-- Otherwise, returns the endpoint and the heap with an updated endpoint timer
grab :: Time.UTCTime -> Time.NominalDiffTime -> EHeap -> Maybe (Endpoint.Endpoint, EHeap)
grab currentTime adjustment heap =
  let isBefore (EndpointSchedule (_, a)) = Time.diffUTCTime currentTime a <= 0
      increaseCheck (EndpointSchedule (a, b)) =
        EndpointSchedule (a, Time.addUTCTime adjustment b)
   in Heap.view heap >>= \(schedule, heap') ->
        if isBefore schedule
          then Just
                 ( unEndpointSchedule schedule
                 , Heap.insert (increaseCheck schedule) heap')
          else Nothing
