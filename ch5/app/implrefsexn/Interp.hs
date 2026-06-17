module Interp where

import Expr
import EnvStore

type FinalAnswer = Store -> (ExpVal, Store)

-- Continuation

data Cont =
    End_Cont
  | Let_Exp_Cont Identifier Exp Env Cont
  | If_Test_Cont Exp Exp Env Cont
  | Diff1_Cont Exp Env Cont
  | Diff2_Cont ExpVal Cont
  | Rator_Cont Exp Env Cont
  | Rand_Cont ExpVal Cont
  | Block_Cont [Exp] Env Cont
  | Set_Cont Identifier Env Cont
  | Unop_Arg_Cont UnaryOp Cont
  | Try_Cont Identifier Exp Env Cont
  | Raise1_Cont Cont

apply_cont :: Cont -> ExpVal -> FinalAnswer
apply_cont End_Cont v store = (v, store)
    
apply_cont (Let_Exp_Cont var body env cont) val1 store1 =
  let (loc,store2) = newref store1 val1
  in value_of_k body (extend_env var loc env) cont store2

apply_cont (If_Test_Cont exp2 exp3 env cont) v store =
  if expval_bool v
  then value_of_k exp2 env cont store
  else value_of_k exp3 env cont store
  
apply_cont (Diff1_Cont exp2 env cont) val1 store =
  value_of_k exp2 env (Diff2_Cont val1 cont) store

apply_cont (Diff2_Cont val1 cont) val2 store =
  let num1 = expval_num val1
      num2 = expval_num val2
  in  apply_cont cont (Num_Val (num1 - num2)) store

apply_cont (Rator_Cont rand env cont) ratorVal store =
  value_of_k rand env (Rand_Cont ratorVal cont) store

apply_cont (Rand_Cont ratorVal cont) randVal store =
  let proc = expval_proc ratorVal in
    apply_procedure_k proc randVal cont store

apply_cont (Block_Cont [] env cont) val store = apply_cont cont val store

apply_cont (Block_Cont (exp1:expList) env cont) val store =
  value_of_k exp1 env (Block_Cont expList env cont) store

apply_cont (Set_Cont x env cont) val1 store1=
  let (loc,store2) = apply_env env store1 x 
      store3 = setref store2 loc val1
  in apply_cont cont (Num_Val 23) store3

apply_cont (Unop_Arg_Cont op cont) val store =
  apply_cont cont (apply_unop op val) store

apply_cont (Try_Cont var handler_exp env cont) val store =
  apply_cont cont val store
                           
apply_cont (Raise1_Cont cont) val store =
  apply_handler val cont store



apply_handler :: ExpVal -> Cont -> FinalAnswer
apply_handler val (Try_Cont var handler_exp env saved_cont) store =
  let (loc,store1) = newref store val in
    value_of_k handler_exp (extend_env var loc env) saved_cont store1

apply_handler val (End_Cont) store =
  error ("Uncaught exception: " ++ show val)

apply_handler val (Let_Exp_Cont x body env cont) store = apply_handler val cont store

apply_handler val (If_Test_Cont exp2 exp3 env cont) store = apply_handler val cont store

apply_handler val (Diff1_Cont exp env cont) store = apply_handler val cont store

apply_handler val (Diff2_Cont val1 cont) store = apply_handler val cont store

apply_handler val (Rator_Cont exp env cont) store = apply_handler val cont store

apply_handler val (Rand_Cont val1 cont) store = apply_handler val cont store

apply_handler val (Block_Cont expList env cont) store = apply_handler val cont store

apply_handler val (Set_Cont var env cont) store = apply_handler val cont store

apply_handler val (Unop_Arg_Cont op cont) store = apply_handler val cont store

apply_handler val (Raise1_Cont cont) store = apply_handler val cont store

apply_unop :: UnaryOp -> ExpVal -> ExpVal 
apply_unop IsZero (Num_Val num)
  | num==0    = Bool_Val True
  | otherwise = Bool_Val False
apply_unop IsNull (List_Val [])  = Bool_Val True
apply_unop IsNull (List_Val _)   = Bool_Val False
apply_unop Car (List_Val (x:_))  = x
apply_unop Cdr (List_Val (_:xs)) = List_Val xs

--
value_of_k :: Exp -> Env -> Cont -> Store -> (ExpVal, Store)

value_of_k (Const_Exp n) env cont store = apply_cont cont (Num_Val n) store

value_of_k (Diff_Exp exp1 exp2) env cont store =
  value_of_k exp1 env (Diff1_Cont exp2 env cont) store

value_of_k (If_Exp exp1 exp2 exp3) env cont store =
  value_of_k exp1 env (If_Test_Cont exp2 exp3 env cont) store

value_of_k (Var_Exp var) env cont store = 
  let (loc,store1) = apply_env env store var 
      val = deref store1 loc 
  in apply_cont cont val store1 

value_of_k (Let_Exp var exp1 body) env cont store =
  value_of_k exp1 env (Let_Exp_Cont var body env cont) store

value_of_k (Letrec_Exp proc_name bound_var proc_body letrec_body) env cont store =
  value_of_k letrec_body (extend_env_rec proc_name bound_var proc_body env) cont store

value_of_k (Proc_Exp var body) env cont store =
  apply_cont cont (Proc_Val (procedure var body env)) store

value_of_k (Call_Exp rator rand) env cont store =
  value_of_k rator env (Rator_Cont rand env cont) store

value_of_k (Block_Exp [exp1]) env cont store = 
  value_of_k exp1 env cont store

value_of_k (Block_Exp (exp1:expList)) env cont store = 
  value_of_k exp1 env (Block_Cont expList env cont) store

value_of_k (Block_Exp []) env cont store = 
  error "value_of_k: Block_Exp is empty"  

value_of_k (Set_Exp var exp1) env cont store = 
  value_of_k exp1 env (Set_Cont var env cont) store

value_of_k (Const_List_Exp nums) env cont store =
  apply_cont cont (List_Val (map Num_Val nums)) store

value_of_k (Unary_Exp op exp1) env cont store =
  value_of_k exp1 env (Unop_Arg_Cont op cont) store

value_of_k (Try_Exp exp var handler_exp) env cont store =
  value_of_k exp env (Try_Cont var handler_exp env cont) store

value_of_k (Raise_Exp exp) env cont store =
  value_of_k exp env (Raise1_Cont cont) store

--
value_of_program :: Exp -> ExpVal

value_of_program exp = 
  let (val, _) =
        value_of_k (Let_Exp "i" (Const_Exp 1) 
                    (Let_Exp "v" (Const_Exp 5)
                      (Let_Exp "x" (Const_Exp 10) exp))) 
          empty_env End_Cont initStore
  in val

--
apply_procedure_k :: Proc -> ExpVal -> Cont -> FinalAnswer
apply_procedure_k proc arg cont store =
   let (loc,store1) = newref store arg in 
    value_of_k (body proc) (extend_env (var proc) loc (saved_env proc)) cont store1
