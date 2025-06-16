{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Scheduler where

import EnvStore
import Queue
import Data.Maybe

-- Threads
-- timeslice = 20

--
-- initialize_scheduler :: Integer -> SchedState
-- initialize_scheduler ticks =
--   SchedState {
--    the_ready_queue = empty_queue,
--    the_final_answer = Nothing,
--    the_max_time_slice = ticks,
--    the_time_remaining = ticks
--   }

-- initScheduler :: SchedState
-- initScheduler =
--   SchedState {
--     the_final_answer = Nothing
--   }

-- place_on_ready_queue :: Thread -> SchedState -> SchedState
-- place_on_ready_queue th scState =
--   scState { the_ready_queue = enqueue (the_ready_queue scState) th }

-- run_next_thread :: Store -> SchedState -> ActorState -> IO (FinalAnswer, Store)
-- run_next_thread store scState actors = do
--   if isempty (the_ready_queue scState)
--   then return (fromJust (the_final_answer scState), store)
--   else do
--     dequeueWithFunIO (the_ready_queue scState)
--      (\first_ready_thread other_ready_threads ->
--         first_ready_thread
--           store
--           ( scState { the_ready_queue = other_ready_threads,
--                       the_time_remaining = the_max_time_slice scState } ) actors )


-- set_final_answer :: SchedState -> ExpVal -> SchedState
-- set_final_answer scState val = scState { the_final_answer = Just val }

-- time_expired :: SchedState -> Bool
-- time_expired scState = the_time_remaining scState==0

-- decrement_timer :: SchedState -> SchedState
-- decrement_timer scState = scState { the_time_remaining = the_time_remaining scState - 1 }

-- Actors

-- run_next_actor :: Store -> SchedState -> ActorState -> IO (FinalAnswer, Store) 
-- run_next_actor store scState (current, q, (next, actorList)) =
--   case actorList of
--     [] -> run_next_thread store scState (current, q, (next, actorList))   -- If no actors are waiting, continue executing self
--     _  -> run_next_actor' (current, q, store, scState) (next, actorList)  -- If there are waiting actors, run_next_actor' with self-info

-- run_next_actor' :: ActorInfo -> ActorSpace -> IO (FinalAnswer, Store)
-- run_next_actor' (current, q, store, scState) (next, x:xs) = do
--   if isempty (the_ready_queue scState)   -- checking the current actor's ready queue
--   then let (nextCurrent, nextQ, nextStore, nextScState) = x                       -- If empty, the current actor is not added to the waiting list
--        in run_next_thread nextStore nextScState (nextCurrent, nextQ, (next, xs))  -- the next actor executes
--   else let actorList1 = (x:xs) ++ [(current, q, store, scState)]                  -- If not empty, the current actor is added to the waiting list
--            (nextCurrent, nextQ, nextStore, nextScState) = head actorList1         -- the next actor executes
--            actorState1 = (nextCurrent, nextQ, (next, tail actorList1))
--        in run_next_thread nextStore nextScState actorState1