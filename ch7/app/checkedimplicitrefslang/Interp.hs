{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interp where

import Expr
import EnvStore

--
value_of :: Exp -> Env -> Store -> (ExpVal, Store)   

value_of (Const_Exp n) env store = (Num_Val n,store)

value_of (Var_Exp var) env store = 
  let (denval,store1) = apply_env env store var 
      loc = denval_loc denval
      val = deref store1 loc
  in  (val, store1)

value_of (Diff_Exp exp1 exp2) env store =
  let (val1,store1) = value_of exp1 env store
      (val2,store2) = value_of exp2 env store1

      num1 = expval_num val1
      num2 = expval_num val2
  in  (Num_Val (num1 - num2), store2)

value_of (Sum_Exp exp1 exp2) env store =
  let (val1,store1) = value_of exp1 env store
      (val2,store2) = value_of exp2 env store1

      num1 = expval_num val1
      num2 = expval_num val2
  in  (Num_Val (num1 + num2), store2)
  
value_of (IsZero_Exp exp) env store =
  let (val1,store1) = value_of exp env store in
    let num1 = expval_num val1 in
      if num1 == 0
      then (Bool_Val True,store1)
      else (Bool_Val False,store1)

value_of (If_Exp exp1 exp2 exp3) env store =
  let (val1,store1) = value_of exp1 env store in
    if expval_bool val1
    then value_of exp2 env store1
    else value_of exp3 env store1

value_of (Let_Exp varExpList body) env store =
  let value_of_let_binding (varList, locList, store) (var,exp) =
        let (val,store') = value_of exp env store
            (loc,store'') = newref store' val
        in  (varList++[var], locList++[loc], store'')

      (varList, locList, store') = 
        foldl value_of_let_binding ([], [], store) varExpList

      env' = extend_env varList locList env
  in value_of body env' store'

value_of (Letrec_Exp letbindings letrec_body) env store =
  value_of letrec_body 
    (extend_env_rec (extend (mkUntypedLetRecBindings letbindings)) env) store
  where extend [] = []
        extend ((proc_name, bound_vars, proc_body):letbindings) =
          (proc_name,bound_vars,proc_body) : extend letbindings

value_of (Proc_Exp var body) env store =
  (Proc_Val (procedure (mkUntyped var) body env),store)

value_of (Call_Exp rator rands) env store =
  let (val1,store1) = value_of rator env store
      proc = expval_proc val1

      (args,store2) = foldl (value_of_arg env) ([],store1) rands
  in apply_procedure proc args store2

value_of (Block_Exp exps) env store =
  let (vals,store1) = foldl value_of_each ([],store) exps
        where
          value_of_each (vals,store) exp =
            let (val,store') = value_of exp env store in
              (vals++[val],store')
  in (last vals,store1)

value_of (Set_Exp x exp) env store =
  let (val1,store1) = value_of exp env store
      (denval,store2) = apply_env env store x
      loc = denval_loc denval
  in  (Num_Val 23,setref store2 loc val1)       -- The dummy value, 23, comes from the EOPL book. :)

value_of (List_Exp exps) env store =
  let (vals,store1) = foldl value_of_each ([],store) exps
        where
          value_of_each (vals,store) exp =
            let (val,store') = value_of exp env store in
              (vals++[val],store')
  in (List_Val vals,store1)

-- Helper function for evaluating args
value_of_arg env (vals,store) arg =
    let (val,store') = value_of arg env store
    in  (vals++[val],store')
--
value_of_program :: Program -> ExpVal
value_of_program prg =
  fst $ value_of prg initEnv initStore

--
-- initEnv in EnvStore.hs
--

--
-- initStore in EnvStore.hs

--
apply_procedure :: Proc -> [ExpVal] -> Store -> (ExpVal,Store)
apply_procedure proc args store = 
  let (locs,store1) = foldl mkRefToArg ([], store) args
        where
          mkRefToArg (locs,store) arg = 
            let (loc,store') = newref store arg in
              (locs++[loc],store')
  in
    value_of (proc_body proc) 
      (extend_env (proc_vars proc) locs (saved_env proc)) store1


-- Utility
mkUntypedLetRecBindings :: LetRecBindings -> [ (Identifier, [Identifier], Exp) ]
mkUntypedLetRecBindings = map (\(_,x,y,z) -> (x, mkUntyped y,z))

mkUntyped :: [(Type, Identifier)] -> [Identifier]
mkUntyped = map snd