{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Semaphores where

-- import EnvStore
-- import Queue
-- import Scheduler

-- new_mutex :: Store -> (Mutex, Store)
-- new_mutex store =
--   let (b,store') = newref store (Bool_Val False)
--       (q,store'') = newref store' (Queue_Val empty_queue)
--   in  (Mutex b q, store'')

-- wait_for_mutex :: Mutex -> Thread -> Store -> SchedState -> ActorState -> IO (FinalAnswer, Store)
-- wait_for_mutex mutex thread store sched actors = do
--   let Mutex ref_to_closed ref_to_wait_queue = mutex
--       closed = deref store ref_to_closed
--       b = expval_bool closed

--       -- Then
--       wait_queue = deref store ref_to_wait_queue
--       q = expval_queue wait_queue
--       q' = enqueue q thread
--       qval = Queue_Val q'
--       then_store' = setref store ref_to_wait_queue qval

--       -- Else
--       else_store' = setref store ref_to_closed (Bool_Val True)
--   if b
--   then run_next_thread then_store' sched actors
--   else thread else_store' sched actors

-- signal_mutex :: Mutex -> Thread -> Store -> SchedState -> ActorState -> IO (FinalAnswer, Store)
-- signal_mutex mutex thread store sched actors = do
--   let Mutex ref_to_closed ref_to_wait_queue = mutex
--       closed = deref store ref_to_closed 
--       b = expval_bool closed
      
--       wait_queue = deref store ref_to_wait_queue 
--       q = expval_queue wait_queue

--   if b
--   then if isempty q
--        then do
--           let store' = setref store ref_to_closed (Bool_Val False)
--           thread store' sched actors
--        else dequeueWithFunIO q
--                     (\first_waiting_thread other_waiting_threads ->
--                        let sched' = place_on_ready_queue first_waiting_thread sched
--                            store' = setref store ref_to_wait_queue
--                                         (Queue_Val other_waiting_threads)
--                        in thread store' sched' actors)
--   else thread store sched actors