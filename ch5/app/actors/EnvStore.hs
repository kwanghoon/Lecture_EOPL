{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric #-}
module EnvStore where

import Expr (Identifier,Exp)
import Data.List(intersperse, intercalate, find)
import Data.Maybe
import Queue

import Control.Distributed.Process (ProcessId, NodeId)

import Data.Binary
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Control.Monad (replicateM)

-- Environment
data Env =
    Empty_env
  | Extend_env ActorId Identifier DenVal Env
  | Extend_env_rec [(Identifier,ActorId,Identifier,Exp)] Env
--  | Serialized_env ActorId Identifier Env
  deriving (Show, Generic)

instance Binary Env

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
--  | Queue_Val {expval_queue :: Queue Thread}  -- (newref queue); newref takes an Expval arg!
  | Actor_Val  {expval_actor :: ActorId}
  | String_Val {expval_string :: String}
  | Loc_Val    {expval_loc :: RemoteLocation} -- location that returned by remote procedure creation
  | Unit_Val  -- for dummy value
  deriving (Generic, Typeable)

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show "<proc>"
  show (List_Val val) = "[" ++ intercalate "," (map show val) ++ "]"
  show (Mutex_Val mutex) = show mutex
--  show (Queue_Val queue) = show "queue"
  show (Actor_Val actor) = "actor" ++ show actor
  show (String_Val str) = str
  show (Loc_Val remoteLoc) = "loc" ++ show (loc remoteLoc) ++ " aid" ++ show (actorId remoteLoc)
  show (Unit_Val) = "dummy"

instance Binary ExpVal where
  put (Num_Val n) = do
    put (0 :: Word8)
    put n
  put (Bool_Val b) = do
    put (1 :: Word8)
    put b
  put (Proc_Val p) = do
    put (2 :: Word8)
    put p
  put (List_Val xs) = do
    put (3 :: Word8)
    put (fromIntegral (length xs) :: Word32)
    mapM_ put xs
  put (Mutex_Val _) =
    error "need to implement"
  put (Actor_Val aid) = do
    put (6 :: Word8)
    put aid
  put (String_Val s) = do
    put (7 :: Word8)
    put s
  put (Loc_Val remoteLoc) = do
    put (8 :: Word8)
    put (loc remoteLoc)
    put (actorId remoteLoc)
  put Unit_Val = put (9 :: Word8)

  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> Num_Val <$> get
      1 -> Bool_Val <$> get
      2 -> Proc_Val <$> get
      3 -> do
        len <- get :: Get Word32
        List_Val <$> replicateM (fromIntegral len) get
      6 -> Actor_Val <$> get
      7 -> String_Val <$> get
      8 -> do
        loc <- get
        aid <- get
        return (Loc_Val (RemoteLocation loc aid))
      9 -> return Unit_Val
      _ -> error "instance Binary ExpVal"

type FinalAnswer = ExpVal 

-- Location
type Location = Integer

-- Denoted values   
type DenVal = Location


-- Procedure values : data structures
data Proc = Procedure {saved_actor :: ActorId, var :: Identifier, body :: Exp, saved_env :: Env}
  deriving (Generic)

instance Binary Proc

procedure :: ActorId -> Identifier -> Exp -> Env -> Proc
procedure actorId var body env = Procedure actorId var body env


-- Remote location : location + actor id
data RemoteLocation = RemoteLocation { loc :: Location, actorId :: ActorId }
  deriving (Show, Typeable, Generic)

instance Binary RemoteLocation

remoteLocation :: Location -> ActorId -> RemoteLocation
remoteLocation loc aid = RemoteLocation { loc = loc, actorId = aid }


-- Mutex values : boolean and thread queue
data Mutex = Mutex Location Location -- binary semaphores: Loc to Bool, Loc to (Queue Thread)
             deriving Show

-- Threads
-- type Thread = Store -> SchedState -> ActorState -> IO (FinalAnswer, Store)

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
type ActorId = ProcessId

data ActorState = ActorState { mainNode :: NodeId } 
  deriving (Generic)

initActorState :: NodeId -> ActorState
initActorState mainNid = ActorState { mainNode = mainNid }

data ActorBehavior = ActorBehavior Identifier Exp Env ActorState
  deriving (Generic, Typeable)

data ActorMessage = StartActor ActorBehavior ActorId
  deriving (Generic, Typeable)

instance Binary ActorState
instance Binary ActorBehavior
instance Binary ActorMessage


-- Messages
data RemoteMessage = 
    RemoteVar (Exp, Env) ActorId
  | RemoteSet (Identifier, ExpVal) ActorId
  | RemoteProc Exp ActorId
  | RemoteCall (ExpVal, ExpVal) ActorId
  deriving (Show, Typeable, Generic)

data ReturnMessage = ReturnMessage ExpVal
  deriving (Show, Typeable, Generic)

instance Binary RemoteMessage
instance Binary ReturnMessage


-- For tuple
bind_vars :: ActorId -> [Identifier] -> [ExpVal] -> Env -> Store -> (Env, Store)
bind_vars _ [] [] env store = (env, store)
bind_vars a (x:xs) (v:vs) env store = 
  let (loc, store') = newref store v
      env' = extend_env a x loc env
  in bind_vars a xs vs env' store'