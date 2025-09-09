{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module EnvStore where

import Expr (Exp,Identifier)
import ActorName(RoleName)
import Data.List(intercalate,find)
import Data.Maybe(listToMaybe)

import Control.Distributed.Process

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable (Typeable)
import Control.Monad (replicateM)
import qualified Data.Map as Map

-- Environment
data Env =
    Empty_env
  | Extend_env ActorId Identifier DenVal Env
  | Extend_env_rec [(Identifier,ActorId,Identifier,Exp,Int)] Env
  | Extend_env_with_actorId Identifier ActorId Env   -- No location! 
  deriving (Generic, Binary)

empty_env :: Env
empty_env = Empty_env

apply_env :: Env -> Store -> Identifier -> Process (ActorId, DenVal, Store)
apply_env Empty_env store search_var = error (search_var ++ " is not found.")
apply_env (Extend_env saved_actor saved_var saved_val saved_env) store search_var
  | search_var==saved_var = return (saved_actor,saved_val,store)
  | otherwise             = apply_env saved_env store search_var
apply_env env@(Extend_env_rec idActoridIdExpPtrList saved_env) store search_var = do
  current <- getSelfPid
  case filter (\(p_name,_,_,_,_) -> p_name == search_var) idActoridIdExpPtrList of
    [] -> apply_env saved_env store search_var
    (p_name, saved_actor, b_var, p_body, ptr):_ ->
      if saved_actor == current
        then do
          let procVal = Proc_Val (procedure saved_actor b_var p_body env)
              (loc, store') = newref store procVal
          return (current, loc, store')
        else do
          send saved_actor (RemoteRec ptr env current)
          loc <- receiveWait [match (\(loc'::DenVal) -> return loc')]
          return (saved_actor, loc, store)
apply_env (Extend_env_with_actorId _ _ saved_env) store search_var =
  apply_env saved_env store search_var  

extend_env :: ActorId -> Identifier -> DenVal -> Env -> Env
extend_env a x v env = Extend_env a x v env

extend_env_rec :: [(Identifier, ActorId, Identifier, Exp, Int)] -> Env -> Env
extend_env_rec idActoridIdExpPtrList env = Extend_env_rec idActoridIdExpPtrList env

extend_env_with_actorId :: Identifier -> ActorId -> Env -> Env
extend_env_with_actorId var aid env = Extend_env_with_actorId var aid env


--
lookup_actorId :: Env -> Identifier -> ActorId 
lookup_actorId Empty_env search_aname = error (search_aname ++ " is not found.")
lookup_actorId (Extend_env saved_actor _ _ saved_env) search_aname =
  lookup_actorId saved_env search_aname
lookup_actorId (Extend_env_rec _ saved_env) search_aname =
  lookup_actorId saved_env search_aname
lookup_actorId (Extend_env_with_actorId saved_aname saved_actor saved_env) search_aname
  | search_aname == saved_aname = saved_actor  
  | otherwise = lookup_actorId saved_env search_aname


-- Expressed values
data ExpVal =
    Num_Val    {expval_num :: Int}
  | Str_Val    {expval_str :: String}
  | Actor_Val  {expval_actor :: ActorId}
  | Bool_Val   {expval_bool :: Bool}
  | Proc_Val   {expval_proc :: Proc}
  | List_Val   {expval_list :: [ExpVal]}
  | Unit_Val  -- for dummy value
  deriving (Generic, Typeable)

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Str_Val str)   = str
  show (Actor_Val aid) = show aid
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show "<proc> var : " ++ show (var proc)
  show (List_Val val)  = "[" ++ intercalate "," (map show val) ++ "]"
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
  put (List_Val xs) = do
    put (5 :: Word8)
    put (fromIntegral (length xs) :: Word32)
    mapM_ put xs
  put Unit_Val = do
    put (6 :: Word8)

  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> Num_Val <$> get
      1 -> Str_Val <$> get
      2 -> Actor_Val <$> get
      3 -> Bool_Val <$> get
      4 -> Proc_Val <$> get
      5 -> do
        len <- get :: Get Word32
        List_Val <$> replicateM (fromIntegral len) get
      6 -> return Unit_Val
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


-- Actors
type ActorId = ProcessId

data ActorState = ActorState 
  { mainNode :: NodeId
  , procMap :: Map.Map Int Exp 
  } 
  deriving (Generic, Binary)

initActorState :: NodeId -> Map.Map Int Exp -> ActorState
initActorState mainNid procMap = ActorState 
  { mainNode = mainNid
  , procMap = procMap
  }


-- Messages
data RemoteMessage = 
    RemoteVar Location ActorId              -- 원격 store 주소
  | RemoteSet (Location, ExpVal) ActorId    -- (원격 store 주소, 할당할 값)
  | RemoteProc Int Env ActorId              -- 생성할 Proc_Exp, Env
  | RemoteCall (ExpVal, ExpVal) ActorId     -- (ratorVal, randVal)
  | RemoteRec Int Env ActorId
  deriving (Generic, Binary, Typeable)

instance Show RemoteMessage where
  show (RemoteVar loc aid) = "RemoteVar loc: " ++ show loc ++ " from: " ++ show aid
  show (RemoteSet (loc, val) aid) = "RemoteSet loc: " ++ show loc ++ " value: " ++ show val ++ " to: " ++ show aid
  show (RemoteProc idx env aid) = "RemoteProc Ptr: " ++ show idx ++ " to: " ++ show aid
  show (RemoteCall (val1, val2) aid) = "RemoteCall rator: " ++ show val1 ++ " value: " ++ show val2 ++ " to: " ++ show aid  
  show (RemoteRec idx env aid) = "RemoteRec Ptr: " ++ show idx ++ " to: " ++ show aid
data ReturnMessage = ReturnMessage ExpVal
  deriving (Generic, Binary, Typeable)



-- For tuple
bind_vars :: ActorId -> [Identifier] -> [ExpVal] -> Env -> Store -> (Env, Store)
bind_vars _ [] [] env store = (env, store)
bind_vars a (x:xs) (v:vs) env store = 
  let (loc, store') = newref store v
      env' = extend_env a x loc env
  in bind_vars a xs vs env' store'