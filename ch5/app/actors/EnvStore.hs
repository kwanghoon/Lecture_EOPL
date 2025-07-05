{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module EnvStore where

import Expr (Exp,Identifier)
import Data.List(intercalate,find)
import Data.Maybe(listToMaybe)

import Control.Distributed.Process (ProcessId, NodeId)

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable (Typeable)
import Control.Monad (replicateM)

-- Environment
data Env =
    Empty_env
  | Extend_env ActorId Identifier DenVal Env
  | Extend_env_rec [(Identifier,ActorId,Identifier,Exp)] Env
  deriving (Generic, Binary)

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
    Num_Val    {expval_num :: Int}
  | Str_Val    {expval_str :: String}
  | Actor_Val  {expval_actor :: ActorId}
  | Bool_Val   {expval_bool :: Bool}
  | Proc_Val   {expval_proc :: Proc}
  | ProcAt_Val {expval_procAt :: ProcAt}
  | List_Val   {expval_list :: [ExpVal]}
  | Loc_Val    {expval_loc :: RemoteLocation} -- location that returned by remote procedure creation
  | Unit_Val  -- for dummy value
  deriving (Generic, Typeable)

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Str_Val str)   = str
  show (Actor_Val aid) = show aid
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show "<proc>"
  show (ProcAt_Val procAt) = show "<procAt>"
  show (List_Val val)  = "[" ++ intercalate "," (map show val) ++ "]"
  show (Loc_Val remoteLoc) = "loc" ++ show (loc remoteLoc) ++ " at" ++ show (actorId remoteLoc)
  show (Unit_Val) = "dummy"

instance Binary ExpVal where
  put (Num_Val n) = do
    put (0 :: Word8)
    put n
  put (Str_Val s) = do
    put (1 :: Word8)
    put s
  put (Actor_Val aid) = do
    put (2 :: Word8)
    put aid
  put (Bool_Val b) = do
    put (3 :: Word8)
    put b
  put (Proc_Val p) = do
    put (4 :: Word8)
    put p
  put (ProcAt_Val p) = do
    put (5 :: Word8)
    put p
  put (List_Val xs) = do
    put (6 :: Word8)
    put (fromIntegral (length xs) :: Word32)
    mapM_ put xs
  put (Loc_Val remoteLoc) = do
    put (7 :: Word8)
    put (loc remoteLoc)
    put (actorId remoteLoc)
  put Unit_Val = do
    put (8 :: Word8)

  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> Num_Val <$> get
      1 -> Str_Val <$> get
      2 -> Actor_Val <$> get
      3 -> Bool_Val <$> get
      4 -> Proc_Val <$> get
      5 -> ProcAt_Val <$> get
      6 -> do
        len <- get :: Get Word32
        List_Val <$> replicateM (fromIntegral len) get
      7 -> do
        loc <- get
        aid <- get
        return $ Loc_Val (RemoteLocation loc aid)
      8 -> return Unit_Val
      _ -> error "Binary instance for ExpVal: unknown tag"

instance Eq ExpVal where
  Num_Val i1   == Num_Val i2   =  i1 == i2
  Str_Val s1   == Str_Val s2   =  s1 == s2
  Bool_Val b1  == Bool_Val b2  =  b1 == b2
  Actor_Val a1 == Actor_Val a2 =  a1 == a2
  _            == _            =  False


type FinalAnswer = ExpVal 

-- Location
type Location = Integer

-- Denoted values   
type DenVal = Location


-- Procedure values : data structures
data Proc = Procedure {saved_actor :: ActorId, var :: Identifier, body :: Exp, saved_env :: Env}
  deriving (Generic, Binary)

procedure :: ActorId -> Identifier -> Exp -> Env -> Proc
procedure actorId var body env = Procedure actorId var body env


-- delayed values
data ProcAt = ProcAt { role_var :: Identifier, delayed_exp :: Exp, delayed_env :: Env }
  deriving (Generic, Binary)

procedureAt :: Identifier -> Exp -> Env -> ProcAt
procedureAt var exp env = ProcAt { role_var = var, delayed_exp = exp, delayed_env = env }


-- Remote location : location + actor id
data RemoteLocation = RemoteLocation { loc :: Location, actorId :: ActorId }
  deriving (Generic, Binary)

remoteLocation :: Location -> ActorId -> RemoteLocation
remoteLocation loc aid = RemoteLocation { loc = loc, actorId = aid }


-- Store
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

lookup_store :: String -> Store -> Maybe ProcAt
lookup_store role (_,store) =
  let matching = [ p | (_, ProcAt_Val p) <- reverse store, role_var p == role ]
  in listToMaybe matching

-- Actors
type ActorId = ProcessId

data ActorState = ActorState { mainNode :: NodeId } 
  deriving (Generic, Binary)

initActorState :: NodeId -> ActorState
initActorState mainNid = ActorState { mainNode = mainNid }

data ActorBehavior = ActorBehavior Identifier Exp Env ActorState
  deriving (Generic, Binary, Typeable)

data ActorMessage = 
  -- stack run actors-exe node ...
    StartActor ActorBehavior ActorId
  -- stack run actors-exe role ...
  | ProcAt1 Exp Env ActorId             -- 원격 함수 호출로 액터 실행 위함
  | ProcAt2 Exp Env ActorId             -- SelectedBehavior로 액터 실행 위함
  | SelectedBehavior Location
  deriving (Generic, Binary, Typeable)


-- Messages
data RemoteMessage = 
    RemoteVar (Exp, Env) ActorId
  | RemoteSet (Identifier, ExpVal) ActorId
  | RemoteProc Exp ActorId
  | RemoteCall (ExpVal, ExpVal) ActorId
  deriving (Generic, Binary, Typeable)

data ReturnMessage = ReturnMessage ExpVal
  deriving (Generic, Binary, Typeable)



-- For tuple
bind_vars :: ActorId -> [Identifier] -> [ExpVal] -> Env -> Store -> (Env, Store)
bind_vars _ [] [] env store = (env, store)
bind_vars a (x:xs) (v:vs) env store = 
  let (loc, store') = newref store v
      env' = extend_env a x loc env
  in bind_vars a xs vs env' store'