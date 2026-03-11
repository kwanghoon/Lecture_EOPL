{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module EnvStore where

import Ref(Location)
import Expr (Identifier,Exp,Type)

import Data.Maybe
import Data.List (elemIndex,lookup)

-- Environment
data Env =
    Empty_env
  | Extend_env [Identifier] [Location] Env
  | Extend_env_rec [(Identifier,[Identifier],Exp)] Env

apply_env :: Env -> Store -> Identifier -> (DenVal, Store)
apply_env Empty_env store search_var = error (search_var ++ " is not found.")
apply_env (Extend_env saved_vars saved_vals saved_env) store search_var = 
  if search_var `elem` saved_vars
    then (Loc_Val $ saved_vals !! (fromJust $ Data.List.elemIndex search_var saved_vars),store)
    else apply_env saved_env store search_var
apply_env (Extend_env_rec idIdExpList saved_env) store search_var
  | isIn      = let (l,store') = newref store procVal in (Loc_Val l,store')
  | otherwise = apply_env saved_env store search_var
  where isIn      = or [ p_name==search_var | (p_name,b_var,p_body) <- idIdExpList ]
        procVal = head [ Proc_Val (procedure b_var p_body (Extend_env_rec idIdExpList saved_env)) 
                       | (p_name,b_var,p_body) <- idIdExpList, p_name==search_var ]

-- Abstract data type interfaces for Env
empty_env :: Env
empty_env = Empty_env

extend_env :: [Identifier] -> [Location] -> Env -> Env
extend_env xs vs env = Extend_env xs vs env

extend_env_rec :: [(Identifier,[Identifier],Exp)] -> Env -> Env
extend_env_rec idIdListExpList env = Extend_env_rec idIdListExpList env

-- Expressed values
data ExpVal =
    Num_Val {expval_num :: Int}
  | Bool_Val {expval_bool :: Bool}
  | Proc_Val {expval_proc :: Proc}
  -- | Ref_Val {expval_loc :: Location}
  | List_Val {expval_list :: [ExpVal]} -- Listof_Val?

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show proc  -- "<proc>"
  show (List_Val list) = "(" ++ showWithSp list ++ ")" -- Todo: print a list as [1,2,3] instead of (1,2,3)??

showWithSp :: [ExpVal] -> String
showWithSp [] = ""
showWithSp [x] = show x
showWithSp (x:xs) = show x ++ " " ++ showWithSp xs  

-- Denoted values
data DenVal = 
    Loc_Val {denval_loc :: Location} -- Ref(ExpVal) 

-- Procedure values : data structures
data Proc = Procedure {proc_vars :: [Identifier], proc_body :: Exp, saved_env :: Env}

instance Show Proc where
  show (Procedure vars body saved_env) = show "<proc>"

procedure :: [Identifier] -> Exp -> Env -> Proc
procedure vars body env = Procedure vars body env

-- In Interp.hs
-- apply_procedure :: Proc -> ExpVal -> ExpVal
  
-- | Store.hs
type Store = (Location, [(Location,ExpVal)]) -- Next new location

newref :: Store -> ExpVal -> (Location,Store)
newref (next,s) v = (next,(next+1,(next,v):s))

deref :: Store -> Location -> ExpVal
deref (next,s) loc =
  case [v | (loc',v) <- s, loc==loc'] of
    (v:_) -> v
    _     -> error ("Location not found: " ++ show loc)

setref :: Store -> Location -> ExpVal -> Store
setref (next,s) loc v = (next,update s)
  where update [] = error ("Invalid reference: " ++ show loc)
        update ((loc',w):s')
          | loc==loc' = (loc,v):s'
          | otherwise = (loc',w):update s'

--
initEnv :: Env
initEnv = extend_env ["i", "v", "x"] [1,2,3] empty_env

initStore :: Store
initStore = (4,[(1, Num_Val 1)
                , (2, Num_Val 5)
                , (3, Num_Val 10)])
