
-- The syntax is based on the implicitrefs language, and
-- the semantics is based on the one for the continuation-based language.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Interp where

import Expr
import EnvStore
import Semaphores
import Scheduler
import Queue (empty_queue)

import Debug.Trace
import Expr (Exp(Send_Exp))
import System.IO (hFlush, stdout)

-- Continuation

data Cont =
    End_Main_Thread_Cont
  | Init_Main_Actor_Cont Cont
  | Zero1_Cont Cont
  | Let_Exp_Cont Identifier Exp Env Cont
  | If_Test_Cont Exp Exp Env Cont
  | Diff1_Cont Exp Env Cont
  | Diff2_Cont ExpVal Cont
  | Rator_Cont Exp Env Cont
  | Rand_Cont ExpVal Cont
  | Unop_Arg_Cont UnaryOp Cont
  | Set_Rhs_Cont Identifier Env Cont
  | Spawn_Cont Cont
  | Wait_Cont Cont
  | Signal_Cont Cont
  | End_Subthread_Cont
  | Send_Cont [Exp] [ExpVal] Env Cont
  | Ready_Cont Cont
  | New_Cont Cont
  | Actor1_Cont Exp Env Cont
  | Actor2_Cont ExpVal Cont

  | Tuple_Cont [Exp] [ExpVal] Env Cont
  | Let_Tuple_Cont [Identifier] Exp Env Cont

  | Remote_Var_Cont ActorId Identifier Env Cont
  | End_Remote_Thread_Cont ActorId
  | Remote_Ready_Cont Cont


apply_cont :: Cont -> ExpVal -> Store -> SchedState -> ActorState -> IO (FinalAnswer, Store)
apply_cont cont val store sched actors = do
  if time_expired sched
  then do
    let sched' = place_on_ready_queue
                   (apply_cont cont val)
                   sched
    run_next_actor store sched' actors  -- run_next_thread
    
  else do
    let sched' = decrement_timer sched
    apply_cont' cont val store sched' actors
    
  where
    apply_cont' End_Main_Thread_Cont v store sched actors =
      let sched' = set_final_answer sched v in 
        run_next_actor store sched' actors -- run_next_thread

    apply_cont' (Init_Main_Actor_Cont cont) v store sched actors =
      let p = expval_proc v
          mainActorId = currentActor actors 
          v1 = Actor_Val mainActorId
      in apply_procedure_k p v1 cont store sched actors

    apply_cont' (Zero1_Cont cont) num1 store sched actors =
      apply_cont cont
        (if expval_num num1 == 0
         then Bool_Val True
         else Bool_Val False) store sched actors

    apply_cont' (Let_Exp_Cont var body env cont) val1 store sched actors =
      let (loc,store') = newref store val1
      in  value_of_k body (extend_env (currentActor actors) var loc env) cont store' sched actors

    apply_cont' (If_Test_Cont exp2 exp3 env cont) v store sched actors =
      if expval_bool v
      then value_of_k exp2 env cont store sched actors
      else value_of_k exp3 env cont store sched actors

    apply_cont' (Diff1_Cont exp2 env cont) val1 store sched actors =
      value_of_k exp2 env (Diff2_Cont val1 cont) store sched actors

    apply_cont' (Diff2_Cont val1 cont) val2 store sched actors =
      let num1 = expval_num val1
          num2 = expval_num val2
      in  apply_cont cont (Num_Val (num1 - num2)) store sched actors

    apply_cont' (Unop_Arg_Cont op cont) val store sched actors = do
      res <- apply_unop op val
      apply_cont cont res store sched actors

    apply_cont' (Rator_Cont rand env cont) ratorVal store sched actors =
      value_of_k rand env (Rand_Cont ratorVal cont) store sched actors

    apply_cont' (Rand_Cont ratorVal cont) randVal store sched actors =
      let proc = expval_proc ratorVal
          current = currentActor actors in
        if actor_name proc == current
        then apply_procedure_k proc randVal cont store sched actors
        else 
             let newThread = \sto sch act -> 
                                apply_procedure_k proc randVal (End_Remote_Thread_Cont current) sto sch act

                 actors' = updateActorSched (actor_name proc) (place_on_ready_queue newThread) actors
             in apply_cont' (Remote_Ready_Cont cont) (Unit_Val) store sched actors'

    apply_cont' (Set_Rhs_Cont var env cont) val store sched actors =
      let saved_actor = lookup_env env var in
       if saved_actor == currentActor actors
       then
            let (loc, store1) = apply_env env store var
                store2 = setref store1 loc val
            in apply_cont cont (Num_Val 23) store2 sched actors
       else
            let newThread = \sto sch act -> 
                                    let (loc, store1) = apply_env env sto var
                                        sto' = setref sto loc val 
                                    in apply_cont' (End_Remote_Thread_Cont (currentActor actors)) (Unit_Val) sto' sch act

                actors' = updateActorSched saved_actor (place_on_ready_queue newThread) actors
            in apply_cont' (Remote_Ready_Cont cont) (Unit_Val) store sched actors'

    apply_cont' (Spawn_Cont saved_cont) val store sched actors =
      let proc1 = expval_proc val
          sched' = place_on_ready_queue
                       (apply_procedure_k proc1 (Num_Val 28) End_Subthread_Cont)
                       sched
      in  apply_cont saved_cont (Num_Val 73) store sched' actors 

    apply_cont' (Wait_Cont saved_cont) val store sched actors =
      wait_for_mutex (expval_mutex val)
        (apply_cont saved_cont (Num_Val 52)) store sched actors

    apply_cont' (Signal_Cont saved_cont) val store sched actors =
      signal_mutex (expval_mutex val)
        (apply_cont saved_cont (Num_Val 53)) store sched actors

    apply_cont' End_Subthread_Cont val store sched actors =
      run_next_actor store sched actors  -- run_next_thread

    apply_cont' (Send_Cont explist vals env saved_cont) val store sched actors =
      let vals' = vals ++ [val] in
        case explist of
          (exp:exps) -> value_of_k exp env (Send_Cont exps vals' env saved_cont) store sched actors
          
          [] -> case vals' of
                  (v:vs) -> let actorId = expval_actor v
                                actors' = sendAllmsg actorId vs actors  -- It can also be implemented using Haskell's foldl
                            in apply_cont saved_cont (Num_Val 42) store sched actors'
 
    apply_cont' (Ready_Cont saved_cont) val store sched actors =
      case readymsg actors of 
        Just (msgVal, actors1) -> 
          let Procedure _ x body env = expval_proc val 
              (next, actorList) = actorSpace actors1 
              (loc, store1) = newref store msgVal 
              env1 = extend_env (currentActor actors) x loc env 
          in value_of_k body env1 saved_cont store1 sched actors1 
        Nothing ->
          let sched1 = 
                place_on_ready_queue
                  (apply_cont' (Ready_Cont saved_cont) val) sched
          in run_next_actor store sched1 actors      

    apply_cont' (New_Cont saved_cont) val store sched actors = 
      let Procedure _ x body env = expval_proc val
          (next, actorList) = actorSpace actors 
          (loc,store1) = newref initStore (Actor_Val next)
          env1 = extend_env next x loc env -- Bug: env must not contain locations to the store!
          sched1 = place_on_ready_queue 
                     (value_of_k body env1 End_Main_Thread_Cont) 
                       (initialize_scheduler timeslice)
          actorList1 = actorList ++ [(next, empty_queue, store1, sched1)]
          actors1 = setActorSpace actors (next+1, actorList1)
      in apply_cont saved_cont (Actor_Val next) store sched actors1

    apply_cont' (Actor1_Cont exp2 env cont) val1 store sched actors =
      value_of_k exp2 env (Actor2_Cont val1 cont) store sched actors

    apply_cont' (Actor2_Cont val1 cont) val2 store sched actors =
      let id = expval_actor val1
          id' = expval_actor val2
      in  if (id == id')
          then apply_cont cont (Bool_Val True) store sched actors
          else apply_cont cont (Bool_Val False) store sched actors

    apply_cont' (Tuple_Cont explist vals env saved_cont) val store sched actors =
      let vals' = vals ++ [val] in
        case explist of
          (exp:exps) -> value_of_k exp env (Tuple_Cont exps vals' env saved_cont) store sched actors
          [] -> let tuple = List_Val vals' 
                in apply_cont saved_cont tuple store sched actors

    apply_cont' (Let_Tuple_Cont vars body env saved_cont) val store sched actors =
      case val of
        List_Val vals -> 
          if vars == [] && null vals
          then value_of_k body env saved_cont store sched actors
          else let (env', store') = bind_vars (currentActor actors) vars vals env store 
               in value_of_k body env' saved_cont store' sched actors

        _ -> error ("LetTuple_Cont: expected a list, got " ++ show val)

    apply_cont' (Remote_Var_Cont actorId var env saved_cont) val store sched actors =
      let newThread = \sto sch act -> 
                              value_of_k (Var_Exp var) env 
                                  (End_Remote_Thread_Cont (currentActor actors)) sto sch act
          actors' = updateActorSched actorId (place_on_ready_queue newThread) actors
      in apply_cont' (Remote_Ready_Cont saved_cont) (Unit_Val) store sched actors'

    apply_cont' (Remote_Ready_Cont saved_cont) val store sched actors =
      case readymsg actors of 
        Just (msgVal, actors1) -> apply_cont saved_cont msgVal store sched actors1
        Nothing -> let sched1 = place_on_ready_queue
                                  (apply_cont' (Remote_Ready_Cont saved_cont) val) sched
                    in run_next_actor store sched1 actors

    apply_cont' (End_Remote_Thread_Cont callerId) val store sched actors =
      let actors' = sendmsg callerId val actors in 
        run_next_actor store sched actors'



-- Todo: Introduce exceptions and define apply_handler to see how complex it is!
-- Todo: Use the monadic style to hide as many global parameters as possible.

apply_unop :: UnaryOp -> ExpVal -> IO ExpVal

apply_unop IsZero (Num_Val num)
  | num==0    = return $ Bool_Val True
  | otherwise = return $ Bool_Val False
apply_unop IsNull (List_Val [])  = return $ Bool_Val True
apply_unop IsNull (List_Val _)   = return $ Bool_Val False
apply_unop Car (List_Val (x:_))  = return $ x
apply_unop Cdr (List_Val (_:xs)) = return $ List_Val xs
apply_unop Print v = do
  putStrLn (show v)
  return Unit_Val
apply_unop Read _ = do
  putStr ">> "
  hFlush stdout
  line <- getLine
  return $ String_Val line
apply_unop op rand = error ("Unknown unary operator: :" ++ show op ++ " " ++ show rand)
--
-- For actor
--   value_of_k :: Exp -> Env -> Cont -> Store -> SchedState 
--                       -> ActorState -> (FinalAnswer, Store)
--

value_of_k :: Exp -> Env -> Cont -> Store -> SchedState -> ActorState -> IO (FinalAnswer, Store)

value_of_k (Const_Exp n) env cont store sched actors =
  apply_cont cont (Num_Val n) store sched actors

value_of_k (Const_List_Exp nums) env cont store sched actors =
  apply_cont cont (List_Val (map Num_Val nums)) store sched actors 

value_of_k (Var_Exp var) env cont store sched actors =
  let saved_actor = lookup_env env var in
    if saved_actor == currentActor actors
    then let (loc, store') = apply_env env store var
             val = deref store' loc
         in apply_cont cont val store' sched actors
    else apply_cont (Remote_Var_Cont saved_actor var env cont) Unit_Val store sched actors

value_of_k (Diff_Exp exp1 exp2) env cont store sched actors =
  value_of_k exp1 env (Diff1_Cont exp2 env cont) store sched actors 

value_of_k (Unary_Exp op exp1) env cont store sched actors =
  value_of_k exp1 env (Unop_Arg_Cont op cont) store sched actors 
  
value_of_k (If_Exp exp1 exp2 exp3) env cont store sched actors =
  value_of_k exp1 env (If_Test_Cont exp2 exp3 env cont) store sched actors 

value_of_k (Let_Exp var exp1 body) env cont store sched actors =
  value_of_k exp1 env (Let_Exp_Cont var body env cont) store sched actors 

value_of_k (Letrec_Exp nameActorNameArgBodyList letrec_body) env cont store sched actors =
  let currentActorId = currentActor actors
      nameActorIdArgBodyList = 
        [ case maybeActorName of 
            Nothing -> (p_name,currentActorId,b_var,p_body) 
            Just actorName ->
              let (loc, store1) = apply_env env store actorName   -- Ignore store1!!
                  actorId  = expval_actor(deref store1 loc)
              in (p_name,actorId,b_var,p_body)
          | (p_name,maybeActorName,b_var,p_body) <- nameActorNameArgBodyList] in 
    value_of_k letrec_body (extend_env_rec nameActorIdArgBodyList env) cont store sched actors 

value_of_k (Proc_Exp (Just actorName) var body) env cont store sched actors = 
  let currentActorId = currentActor actors
      (loc, store1) = apply_env env store actorName   -- Ignore store1!!
      remoteActorId = expval_actor (deref store1 loc) in
      if remoteActorId == currentActorId
      then apply_cont cont (Proc_Val (procedure currentActorId var body env)) store sched actors
      else
           let newThread = \sto sch act -> 
                          value_of_k (Proc_Exp Nothing var body) env 
                                  (End_Remote_Thread_Cont currentActorId) sto sch act

               actors' = updateActorSched remoteActorId (place_on_ready_queue newThread) actors
           in apply_cont (Remote_Ready_Cont cont) (Unit_Val) store sched actors'

value_of_k (Proc_Exp Nothing var body) env cont store sched actors =
  apply_cont cont (Proc_Val (procedure (currentActor actors) var body env)) store sched actors    

value_of_k (Call_Exp rator rand) env cont store sched actors =
  value_of_k rator env (Rator_Cont rand env cont) store sched actors 
  
value_of_k (Block_Exp [exp]) env cont store sched actors =
  value_of_k exp env cont store sched actors 

value_of_k (Block_Exp (exp:exps)) env cont store sched actors =
  value_of_k (Call_Exp (Proc_Exp Nothing "$dummy" (Block_Exp exps)) exp) env cont store sched actors 

value_of_k (Block_Exp []) env cont store sched actors =
  error "Unexpected empty block"

value_of_k (Set_Exp x exp) env cont store sched actors =
    value_of_k exp env (Set_Rhs_Cont x env cont) store sched actors 

value_of_k (Spawn_Exp exp) env cont store sched actors =
  value_of_k exp env (Spawn_Cont cont) store sched actors 

value_of_k Yield_Exp env cont store sched actors =
  let yieldsched =
        place_on_ready_queue
          (apply_cont cont (Num_Val 99))
          sched
  in  run_next_actor store yieldsched actors  -- run_next_thread

value_of_k Mutex_Exp env cont store sched actors =
  let (mutex, store') = new_mutex store in
    apply_cont cont (Mutex_Val mutex) store' sched actors 

value_of_k (Wait_Exp exp) env cont store sched actors =
  value_of_k exp env (Wait_Cont cont) store sched actors 

value_of_k (Signal_Exp exp) env cont store sched actors =
  value_of_k exp env (Signal_Cont cont) store sched actors 

-- For actors
value_of_k (Send_Exp (exp:exps)) env cont store sched actors =
  value_of_k exp env (Send_Cont exps [] env cont) store sched actors

value_of_k (Ready_Exp exp) env cont store sched actors =
  value_of_k exp env (Ready_Cont cont) store sched actors

value_of_k (New_Exp exp) env cont store sched actors =
  value_of_k exp env (New_Cont cont) store sched actors

value_of_k (Eq_Actor_Exp exp1 exp2) env cont store sched actors =
  value_of_k exp1 env (Actor1_Cont exp2 env cont) store sched actors

-- For tuple
value_of_k (Tuple_Exp []) env cont store sched actors =
  apply_cont cont (List_Val []) store sched actors

value_of_k (Tuple_Exp (exp:exps)) env cont store sched actors =
  value_of_k exp env (Tuple_Cont exps [] env cont) store sched actors

value_of_k (LetTuple_Exp vars exp1 exp2) env cont store sched actors =
  value_of_k exp1 env (Let_Tuple_Cont vars exp2 env cont) store sched actors

value_of_k (Log_Exp str exp) env cont store sched actors =
  trace ("[actor" ++ show (currentActor actors)++"]") $
  trace str $ 
  value_of_k exp env cont store sched actors

value_of_k exp _ _ _ _ _ =
  error $ "Unknown expression in value_of_k" ++ show exp
  

--
value_of_program :: Exp -> Integer -> IO ExpVal

value_of_program exp timeslice = do
  (finalVal, _) <- value_of_k exp initEnv End_Main_Thread_Cont
                     initStore (initialize_scheduler timeslice) initialActorState
  return finalVal


--
initEnv = empty_env

--
apply_procedure_k :: Proc -> ExpVal -> Cont -> Store -> SchedState -> ActorState -> IO (FinalAnswer, Store)
apply_procedure_k proc arg cont store sched actors = do
  let (loc,store') = newref store arg
  value_of_k (body proc) (extend_env (currentActor actors) (var proc) loc (saved_env proc)) cont store' sched actors