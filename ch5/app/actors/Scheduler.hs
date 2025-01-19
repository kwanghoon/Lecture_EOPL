{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Scheduler where

import EnvStore
import Queue
import Data.Maybe

-- Threads
timeslice = 20

--
initialize_scheduler :: Integer -> SchedState
initialize_scheduler ticks =
  SchedState {
   the_ready_queue = empty_queue,
   the_final_answer = Nothing,
   the_max_time_slice = ticks,
   the_time_remaining = ticks
  }

place_on_ready_queue :: Thread -> SchedState -> SchedState
place_on_ready_queue th scState =
  scState { the_ready_queue = enqueue (the_ready_queue scState) th }

run_next_thread :: Store -> SchedState -> ActorState -> (FinalAnswer, Store)
run_next_thread store scState actors =
  if isempty (the_ready_queue scState)
  then (fromJust (the_final_answer scState), store)
  else
    dequeueWithFun (the_ready_queue scState)
     (\first_ready_thread other_ready_threads ->
        first_ready_thread
          store
          ( scState { the_ready_queue = other_ready_threads,
                      the_time_remaining = the_max_time_slice scState } ) actors )

set_final_answer :: SchedState -> ExpVal -> SchedState
set_final_answer scState val = scState { the_final_answer = Just val }

time_expired :: SchedState -> Bool
time_expired scState = the_time_remaining scState==0

decrement_timer :: SchedState -> SchedState
decrement_timer scState = scState { the_time_remaining = the_time_remaining scState - 1 }

-- Actors
-- run_next_actor :: Store -> SchedState -> ActorState -> (FinalAnswer, Store) 
-- run_next_actor store scState (current, q, actorSpace) =
--   run_next_actor' (current, q, store, scState) actorSpace

-- run_next_actor' :: ActorInfo -> ActorSpace -> (FinalAnswer, Store)
-- run_next_actor' (currActor, _, _, _) (next, []) = 
--   error ("Blocking: " ++ show currActor) -- Todo: better way to handle this?

-- run_next_actor' currentActorInfo (next, actorList) = 
--   let actorList1 = actorList ++ [currentActorInfo]
--       (nextCurrent, nextQ, nextStore, nextScState) = head actorList1 
--       actorState1 = (nextCurrent, nextQ, (next, tail actorList1))
--   in if isempty (the_ready_queue nextScState)
--      then if null (tail actorList1)
--           then (fromJust (the_final_answer nextScState), nextStore)
--           else run_next_actor' currentActorInfo (next, tail actorList1)
--      else run_next_thread nextStore nextScState actorState1

run_next_actor :: Store -> SchedState -> ActorState -> (FinalAnswer, Store) 
run_next_actor store scState (current, q, (next, actorList)) =
  case actorList of
    [] -> run_next_thread store scState (current, q, (next, actorList))
    _  -> run_next_actor' (current, q, store, scState) (next, actorList)

run_next_actor' :: ActorInfo -> ActorSpace -> (FinalAnswer, Store)
run_next_actor' (current, q, store, scState) (next, x:xs) =
  if isempty (the_ready_queue scState)
  then let (nextCurrent, nextQ, nextStore, nextScState) = x
       in if isempty (the_ready_queue nextScState)
          then run_next_actor nextStore nextScState (nextCurrent, nextQ, (next, xs))
          else run_next_thread nextStore nextScState (nextCurrent, nextQ, (next, xs))

  else let actorList1 = (x:xs) ++ [(current, q, store, scState)] -- currentActorInfo
           (nextCurrent, nextQ, nextStore, nextScState) = head actorList1
           actorState1 = (nextCurrent, nextQ, (next, tail actorList1))
       in if isempty (the_ready_queue nextScState)
          then run_next_actor nextStore nextScState actorState1
          else run_next_thread nextStore nextScState actorState1