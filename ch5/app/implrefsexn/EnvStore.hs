{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module EnvStore where

import Ref(Location)
import Expr (Identifier,Exp)
import Data.List(intersperse)

-- Environment
data Env =
    Empty_env
  | Extend_env Identifier DenVal Env
  | Extend_env_rec Identifier Identifier Exp Env

empty_env :: Env
empty_env = Empty_env

apply_env :: Env -> Store -> Identifier -> (DenVal, Store)
apply_env Empty_env store search_var = error (search_var ++ " is not found.")
apply_env (Extend_env saved_var saved_val saved_env) store search_var
  | search_var==saved_var = (saved_val,store)
  | otherwise             = apply_env saved_env store search_var
apply_env (Extend_env_rec f x exp saved_env) store search_var
  | f==search_var = 
      let procVal = Proc_Val (procedure x exp (Extend_env_rec f x exp saved_env))
      in newref store procVal
  | otherwise     = apply_env saved_env store search_var

extend_env :: Identifier -> DenVal -> Env -> Env
extend_env x v env = Extend_env x v env

extend_env_rec :: Identifier -> Identifier -> Exp -> Env -> Env
extend_env_rec f x exp env = Extend_env_rec f x exp env

-- Expressed values
data ExpVal =
    Num_Val {expval_num :: Int}
  | Bool_Val {expval_bool :: Bool}
  | Proc_Val {expval_proc :: Proc}
  -- | Ref_Val {expval_loc :: Location}
  | List_Val {expval_list :: [ExpVal]}  

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show "<proc>"
  show (List_Val nums) = show "[" ++ concat (intersperse "," (map show nums)) ++ show "]"

-- Denoted values
type DenVal = Location   

-- Procedure values : data structures
data Proc = Procedure {var :: Identifier, body :: Exp, saved_env :: Env}

procedure :: Identifier -> Exp -> Env -> Proc
procedure var body env = Procedure var body env

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

initStore :: Store
initStore = (1,[])
