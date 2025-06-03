module Queue where

type Queue a = [a]

empty_queue :: Queue a
empty_queue = []

isempty :: Queue a -> Bool
isempty queue = null queue

enqueue :: Queue a -> a -> Queue a
enqueue queue elem = queue ++ [elem]

dequeueWithFun :: Queue a -> (a -> Queue a -> b) -> b 
dequeueWithFun queue f = f (head queue) (tail queue)

dequeueWithFunIO :: Queue a -> (a -> Queue a -> IO b) -> IO b
dequeueWithFunIO queue f
  | isempty queue = error "Empty queue"
  | otherwise = f (head queue) (tail queue)

  -- Note that dequeue is used in run_next_thread:Schedule.hs and
  -- singal_mutex:Semaphore.hs where
  --
  --    b = Store -> SchedState -> (FinalAnswer, Store).
  --
  -- One can implement dequeue as usual
  --
  --    dequeue :: Queue a -> (a, Queue a)
  --
  -- so that dequeue queue f can be rewritten as
  --
  --    let (head, queue') = dequeue queue in
  --      f head queue'.
  --


-- Replaced by some nonstandard interface, dequeueWithFun. 
dequeue :: Queue a -> (a, Queue a)
dequeue q = if isempty q
            then error "dequeue: fail to dequeue from the empty queue"
            else (head q, tail q)