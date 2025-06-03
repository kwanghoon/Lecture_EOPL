{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module EnvStore where

import Expr (Identifier,Exp)
import Data.List(intersperse)
import Data.Maybe
import Queue

-- Environment
data Env =
    Empty_env
  | Extend_env ActorId Identifier DenVal Env
  | Extend_env_rec [(Identifier,ActorId,Identifier,Exp)] Env

empty_env :: Env
empty_env = Empty_env

apply_env :: Env -> Store -> Identifier -> (DenVal, Store)
apply_env Empty_env store search_var = error (search_var ++ " is not found.")
apply_env (Extend_env _ saved_var saved_val saved_env) store search_var
  | search_var==saved_var = (saved_val,store)
  | otherwise             = apply_env saved_env store search_var
apply_env (Extend_env_rec idActoridIdExpList saved_env) store search_var
  | isIn      = let (loc, store') = newref store procVal
                in (loc, store')
  | otherwise = apply_env saved_env store search_var
  where isIn      = or [ p_name==search_var | (p_name,saved_actor,b_var,p_body) <- idActoridIdExpList ]
        procVal = head [ Proc_Val (procedure saved_actor b_var p_body (Extend_env_rec idActoridIdExpList saved_env)) 
                       | (p_name,saved_actor,b_var,p_body) <- idActoridIdExpList, p_name==search_var ]

extend_env :: ActorId -> Identifier -> DenVal -> Env -> Env
extend_env a x v env = Extend_env a x v env

extend_env_rec :: [(Identifier, ActorId, Identifier, Exp)] -> Env -> Env
extend_env_rec idActoridIdExpList env = Extend_env_rec idActoridIdExpList env

-- lookup_env: 변수가 정의된 위치(액터 id)만 반환
lookup_env :: Env -> Identifier -> ActorId
lookup_env Empty_env search_var = error (search_var ++ " is not found.")

lookup_env (Extend_env saved_actor saved_var _ saved_env) search_var
  | search_var == saved_var = saved_actor
  | otherwise               = lookup_env saved_env search_var

lookup_env (Extend_env_rec idActoridIdExpList saved_env) search_var =
  case [ saved_actor | (p_name, saved_actor, _, _) <- idActoridIdExpList, p_name == search_var ] of
    (saved_actor:_) -> saved_actor
    []     -> lookup_env saved_env search_var

-- Expressed values
data ExpVal =
    Num_Val   {expval_num  :: Int}
  | Bool_Val  {expval_bool :: Bool}
  | Proc_Val  {expval_proc :: Proc}
  | List_Val  {expval_list :: [ExpVal]}
  | Mutex_Val {expval_mutex :: Mutex }        -- Mutex {Loc to Bool, Loc to Queue Thread}
  | Queue_Val {expval_queue :: Queue Thread}  -- (newref queue); newref takes an Expval arg!
  | Actor_Val {expval_actor :: Integer}
  | String_Val {expval_string :: String}
  | Unit_Val  -- for dummy value

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show "<proc>"
  show (List_Val nums) = show "[" ++ concat (intersperse "," (map show nums)) ++ show "]"
  show (Mutex_Val mutex) = show mutex
  show (Queue_Val queue) = show "queue"
  show (Actor_Val actor) = "actor" ++ show actor
  show (String_Val str) = show str
  show (Unit_Val) = "dummy"

type FinalAnswer = ExpVal 

-- Location
type Location = Integer

-- Denoted values   
type DenVal = Location

-- Procedure values : data structures
data Proc = Procedure {actor_name :: ActorId, var :: Identifier, body :: Exp, saved_env :: Env}

procedure :: ActorId -> Identifier -> Exp -> Env -> Proc
procedure actorId var body env = Procedure actorId var body env

-- Mutex values : boolean and thread queue
data Mutex = Mutex Location Location -- binary semaphores: Loc to Bool, Loc to (Queue Thread)
             deriving Show

-- Threads
type Thread = Store -> SchedState -> ActorState -> IO (FinalAnswer, Store)

-- Scheduler states
data SchedState =
  SchedState {
   the_ready_queue :: Queue Thread,
   the_final_answer :: Maybe FinalAnswer,
   the_max_time_slice :: Integer,
   the_time_remaining :: Integer
  }

-- In Interp.hs
-- apply_procedure :: Proc -> ExpVal -> ExpVal
  
type Store = (Location, [(Location,ExpVal)]) -- Next new location

newref :: Store -> ExpVal -> (Location,Store)
newref store@(next,s) v = (next,(next+1,(next,v):s))

deref :: Store -> Location -> ExpVal
deref store@(next,s) loc =
  case [v | (loc',v) <- s, loc==loc'] of
    (v:_) -> v
    _     -> error ("Location not found: " ++ show loc)

setref :: Store -> Location -> ExpVal -> Store
setref store@(next,s) loc v = (next,update s)
  where update [] = error ("Invalid reference: " ++ show loc)
        update ((loc',w):s')
          | loc==loc' = (loc,v):s'
          | otherwise = (loc',w):update s'

initStore :: Store
initStore = (1,[])

-- Actors
type ActorId = Integer

-- (next actor id, actors)
type ActorSpace = (ActorId, [ ActorInfo ])
type ActorInfo  = (ActorId, Queue ExpVal, Store, SchedState)

-- (current actor id, message queue, actor space)
type ActorState = (ActorId, Queue ExpVal, ActorSpace)

currentActor :: ActorState -> ActorId
currentActor (actor,_,_) = actor

msgQueue :: ActorState -> Queue ExpVal
msgQueue (_,queue,_) = queue

actorSpace :: ActorState -> ActorSpace
actorSpace (_,_,space) = space

setActorSpace :: ActorState -> ActorSpace -> ActorState
setActorSpace (actor,queue,_) space = (actor,queue,space)

-- 0 for the main actor, 1 for the next actors to be created
initialActorState :: ActorState
initialActorState = (0, empty_queue, (1, []))

-- lookup and remove for Remote
-- extractActor :: ActorId -> ActorState -> (ActorInfo, ActorState)
-- extractActor name (current, q, (next, actorList)) =
--   let (found, rest) = extractActor' name actorList
--   in (found, (current, q, (next, rest)))

-- extractActor' :: ActorId -> [ActorInfo] -> (ActorInfo, [ActorInfo])
-- extractActor' _ [] = error "Actor not found"
-- extractActor' name (info@(n,_,_,_):rest)
--   | name == n = (info, rest)
--   | otherwise = let (found, rest') = extractActor' name rest
--                 in (found, info : rest')

-- lookup and update target actor's scheduler for Remote
updateActorSched :: ActorId -> (SchedState -> SchedState) -> ActorState -> ActorState
updateActorSched search_Id f (current, q, (next, actorList)) =
  let updatedActorList = updateActorSched' search_Id f actorList
  in (current, q, (next, updatedActorList))
  where
    updateActorSched' _ _ [] = error ("Actor not found : " ++ show search_Id)
    updateActorSched' search_Id f (info@(id, q, store, sched):rest)
      | search_Id == id = (id, q, store, f sched) : rest
      | otherwise = info : updateActorSched' search_Id f rest


-- For actor
--   value_of_k :: Exp -> Env -> Cont -> Store -> SchedState 
--                       -> ActorState -> (FinalAnswer, Store)


-- Actor별로 Store를 가지고 있음 Store
-- Actor별로 메시지 큐를 가지고 있음 Queue ExpVal

sendmsg :: ActorId -> ExpVal -> ActorState -> ActorState 
sendmsg to v (current, q, (next, actorList))
  | to == current = (current, enqueue q v, (next, actorList))
  | otherwise = (current, q, (next, sendmsg' to v actorList))

sendmsg' :: ActorId -> ExpVal -> [ActorInfo] -> [ActorInfo]
sendmsg' to v [] = [] -- error ("Unknown actor for send: " ++ show to ++ ", " ++ show v)
sendmsg' to v ((name, q, store, sched):actorList)
  | to == name = (name, enqueue q v, store, sched) : actorList
  | otherwise = (name, q, store, sched) : sendmsg' to v actorList

sendAllmsg :: ActorId -> [ExpVal] -> ActorState -> ActorState
sendAllmsg _ [] actors = actors
sendAllmsg to (v:vs) actors =
  let actors1 = sendmsg to v actors
  in sendAllmsg to vs actors1

readymsg :: ActorState -> Maybe (ExpVal, ActorState)
readymsg (current, q, actorSpace) 
  | isempty q = Nothing 
  | otherwise = let (v, q1) = dequeue q in Just (v, (current, q1, actorSpace))

-- For tuple
bind_vars :: ActorId -> [Identifier] -> [ExpVal] -> Env -> Store -> (Env, Store)
bind_vars _ [] [] env store = (env, store)
bind_vars a (x:xs) (v:vs) env store = 
  let (loc, store') = newref store v
      env' = extend_env a x loc env
  in bind_vars a xs vs env' store'