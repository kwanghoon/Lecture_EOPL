
-- The syntax is based on the implicitrefs language, and
-- the semantics is based on the one for the continuation-based language.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interp where

import Expr
import EnvStore
import Semaphores
import Queue (empty_queue)
import NodeRegistry
import DynamicMessage

import Debug.Trace
import Expr (Exp(Send_Exp))
import System.IO (hFlush, stdout)

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)
import Network.Transport (EndPointAddress(..))
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS

import Data.Typeable ()
import GHC.Generics ()
import Data.Binary ()
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, readTVar, modifyTVar')


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
  | Rand_Cont ExpVal Env Cont
  | Unop_Arg_Cont UnaryOp Cont
  | Set_Rhs_Cont Identifier Env Cont
  | Spawn_Cont Cont
  | Wait_Cont Cont
  | Signal_Cont Cont
  | End_Subthread_Cont
  | Send_Cont [Exp] [ExpVal] Env Cont
  | Ready_Cont Cont
  | RemoteReady_Cont Cont
  | New_Cont Cont
  | Actor1_Cont Exp Env Cont
  | Actor2_Cont ExpVal Cont

  | Tuple_Cont [Exp] [ExpVal] Env Cont
  | Let_Tuple_Cont [Identifier] Exp Env Cont
  | Append_Cont Identifier Env Cont

apply_cont :: Cont -> ExpVal -> Store -> ActorState -> Process (FinalAnswer, Store)

apply_cont End_Main_Thread_Cont v store actors = do
  -- current <- getSelfPid
  -- liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] End_Main_Thread_Cont, Val: " ++ show v
  return (v, store)

apply_cont (Init_Main_Actor_Cont cont) v store actors = do
  current <- getSelfPid
  let p = expval_proc v
      v1 = Actor_Val current
  apply_procedure_k p v1 cont store actors

apply_cont (Zero1_Cont cont) num1 store actors =
  apply_cont cont
    (if expval_num num1 == 0
     then Bool_Val True
     else Bool_Val False) store actors

apply_cont (Let_Exp_Cont var body env cont) val1 store actors = do
  current <- getSelfPid
  case val1 of
    Loc_Val remoteLoc -> do
      -- liftIO $ putStrLn $ "[Process@" ++ show current ++ "] extend_env " ++ show (actorId remoteLoc) ++ " " ++ var ++ " " ++ show (loc remoteLoc)
      value_of_k body (extend_env (actorId remoteLoc) var (loc remoteLoc) env) cont store actors
    _ -> do
      let (loc,store') = newref store val1
      -- liftIO $ putStrLn $ "[Process@" ++ show current ++ "] extend_env  " ++ show current ++ " " ++ var ++ " " ++ show loc
      value_of_k body (extend_env current var loc env) cont store' actors

apply_cont (If_Test_Cont exp2 exp3 env cont) v store actors =
  if expval_bool v
  then value_of_k exp2 env cont store actors
  else value_of_k exp3 env cont store actors

apply_cont (Diff1_Cont exp2 env cont) val1 store actors =
  value_of_k exp2 env (Diff2_Cont val1 cont) store actors

apply_cont (Diff2_Cont val1 cont) val2 store actors =
  let num1 = expval_num val1
      num2 = expval_num val2
  in apply_cont cont (Num_Val (num1 - num2)) store actors

apply_cont (Unop_Arg_Cont op cont) val store actors = do
  res <- apply_unop op val
  apply_cont cont res store actors

apply_cont (Rator_Cont rand env cont) ratorVal store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] (Rator_Cont rand env cont), Val: " ++ show ratorVal
  value_of_k rand env (Rand_Cont ratorVal env cont) store actors

apply_cont (Rand_Cont ratorVal env cont) randVal store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] (Rand_Cont ratorVal env cont), Val: " ++ show randVal
  let proc = expval_proc ratorVal
      actorId = saved_actor proc
  if actorId == current
  then do
    --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] then -> apply_procedure_k"
    apply_procedure_k proc randVal cont store actors
  else do
    --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] else -> send RemoteCall"
    send actorId (RemoteCall (ratorVal, randVal) current)
    apply_cont (RemoteReady_Cont cont) (Proc_Val (procedure current "$dummy" (Const_Exp 42) env)) store actors

apply_cont (Set_Rhs_Cont var env cont) val store actors = do
  let saved_actor = lookup_env env var
  current <- getSelfPid
  --liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] (Set_Rhs_Cont var env cont), Val: " ++ show val
  if saved_actor == current
  then
      let (loc, store1) = apply_env env store var
          store2 = setref store1 loc val
      in apply_cont cont (Unit_Val) store2 actors
  else do
    --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] send RemoteSet to" ++ show saved_actor
    send saved_actor (RemoteSet (var, val) current)
    apply_cont (RemoteReady_Cont cont) (Proc_Val (procedure current "$dummy" (Const_Exp 42) env)) store actors

apply_cont (Spawn_Cont saved_cont) val store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] (Spawn_Cont saved_cont), Val: " ++ show val
  let Procedure _ x body env = expval_proc val
  -- 로컬에서 액터를 생성(spawnLocal)
  newPid <- spawnLocal $ do
              self <- getSelfPid
              --liftIO $ putStrLn $ "[Process@" ++ show self ++ "] SpawnLocal"
              let (loc, store1) = newref initStore (Actor_Val self)
                  env1 = extend_env self x loc env
              _ <- value_of_k body env1 End_Main_Thread_Cont store1 actors
              forever $ liftIO $ threadDelay maxBound
  apply_cont saved_cont (Actor_Val newPid) store actors

    -- apply_cont (Spawn_Cont saved_cont) val store actors =
    --   let proc1 = expval_proc val
    --      ' = place_on_ready_queue
    --                    (apply_procedure_k proc1 (Num_Val 28) End_Subthread_Cont)
    --                   
    --   in  apply_cont saved_cont (Num_Val 73) store' actors 

    -- apply_cont (Wait_Cont saved_cont) val store actors =
    --   wait_for_mutex (expval_mutex val)
    --     (apply_cont saved_cont (Num_Val 52)) store actors

    -- apply_cont (Signal_Cont saved_cont) val store actors =
    --   signal_mutex (expval_mutex val)
    --     (apply_cont saved_cont (Num_Val 53)) store actors

-- apply_cont End_Subthread_Cont val store actors =
--   run_next_actor store actors  -- run_next_thread

apply_cont (Send_Cont explist vals env saved_cont) val store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] (Send_Cont explist vals env saved_cont), Val: " ++ show val
  let vals' = vals ++ [val]
  case explist of
    (exp:exps) -> value_of_k exp env (Send_Cont exps vals' env saved_cont) store actors
    [] -> case vals' of
            (v:vs) -> do
              case v of
                Actor_Val pid -> do
                  let pid = expval_actor v
                  mapM_ (send pid) vs       -- send :: (Serializable msg) => ProcessId -> msg -> Process ()
                  apply_cont saved_cont (Unit_Val) store actors
                List_Val pids -> do
                  -- 리스트 안의 각 Actor_Val에 메시지 전송
                  let maybePids = [ expval_actor av | av@(Actor_Val _) <- pids ]
                  --liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] Broadcasting to " ++ show maybePids
                  mapM_ (\pid -> mapM_ (send pid) vs) maybePids
                  apply_cont saved_cont Unit_Val store actors

apply_cont (Ready_Cont saved_cont) val store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] (Ready_Cont saved_cont), Val: " ++ show val
  let Procedure _ _ _ env = expval_proc val
  receiveWait
    [ match $ \(msg :: RemoteMessage) -> do
        case msg of
          -- 변수 read 요청 처리
          RemoteVar (Var_Exp var, saved_env) requester -> do
            --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] RemoteVar received from " ++ show requester ++ " for variable " ++ show var
            (returnVal, store1) <- value_of_k (Var_Exp var) saved_env End_Main_Thread_Cont store actors
            send requester (ReturnMessage returnVal)
            apply_cont (Ready_Cont saved_cont) val store1 actors
          -- 변수 write 요청 처리
          RemoteSet (var, val') requester -> do
            --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] RemoteSet received from " ++ show requester ++ " for variable " ++ show var
            (_, updatedStore) <- let (loc, store1) = apply_env env store var
                                     store2 = setref store1 loc val'
                                 in apply_cont End_Main_Thread_Cont (Unit_Val) store2 actors
            send requester (ReturnMessage Unit_Val)
            apply_cont (Ready_Cont saved_cont) val updatedStore actors
          -- 프로시저 생성 요청 처리
          RemoteProc (Proc_Exp Nothing var body) requester -> do
            --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] RemoteProc received from " ++ show requester
            (returnVal, store1) <- value_of_k (Proc_Exp Nothing var body) env End_Main_Thread_Cont store actors
            let (loc,store2) = newref store1 returnVal
                returnVal' = Loc_Val (remoteLocation loc current)
            send requester (ReturnMessage returnVal')
            apply_cont (Ready_Cont saved_cont) val store2 actors
          -- 프로시저 호출 요청 처리
          RemoteCall (ratorVal, randVal) requester -> do
            --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] RemoteCall received from " ++ show requester
            let proc = expval_proc ratorVal
            (returnVal, store1) <- apply_procedure_k proc randVal End_Main_Thread_Cont store actors
            send requester (ReturnMessage returnVal)
            apply_cont (Ready_Cont saved_cont) val store1 actors
      ,
      match $ \(msg :: ExpVal) -> do
        --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] ExpVal received"
        let Procedure _ x body env = expval_proc val
            (loc, store1) = newref store msg
        let env1 = extend_env current x loc env
        value_of_k body env1 saved_cont store1 actors
      ,
      match $ \(RequestEnvStore from :: DynamicMessage) -> do
        send from (RespondEnvStore (env, store))
        apply_cont (Ready_Cont saved_cont) val store actors
    ]

apply_cont (RemoteReady_Cont saved_cont) val store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] (RemoteReady_Cont saved_cont), Val: " ++ show val
  let Procedure _ _ _ env = expval_proc val
  receiveWait
    [ match $ \(msg :: ReturnMessage) -> do
        --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] ReturnMessage received"
        let ReturnMessage returnVal = msg
        apply_cont saved_cont returnVal store actors
    ]

-- apply_cont (New_Cont saved_cont) val store actors = do
--   current <- getSelfPid
--   --liftIO $ putStrLn $ "[apply_cont@" ++ show current ++ "] (New_Cont saved_cont), Val: " ++ show val
--   let Procedure _ x body env = expval_proc val
--       mainNid = mainNode actors
--   -- 메인 노드의 "nodeRegistry" 프로세스 id 요청
--   whereisRemoteAsync mainNid "nodeRegistry"
--   pidReply <- expectTimeout 5000000
--   case pidReply of
--     Just (WhereIsReply "nodeRegistry" (Just registryPid)) -> do
--       -- "nodeRegistry"에 사용 가능한 노드 요청
--       send registryPid (RequestNode current)
--       receiveWait
--         [ match $ \(msg :: NodeMessage) -> 
--             case msg of
--               -- 할당된 노드가 있으면 "nodeListener" 프로세스 id 요청
--               AssignNode nid -> do
--                   whereisRemoteAsync nid "nodeListener"
--                   m <- expectTimeout 5000000
--                   case m of
--                     Just (WhereIsReply "nodeListener" (Just listenerPid)) -> do
--                           -- "nodeListener"에 StartActor 메시지를 보내 원격에서 실행
--                           --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] Send message to assigned node"
--                           send listenerPid (StartActor (ActorBehavior x body env actors) current)
--                           response <- expectTimeout 5000000
--                           case response of
--                             Just newPid -> apply_cont saved_cont (Actor_Val newPid) store actors
--                             Nothing     -> error "Did not receive PID from nodeListener"
--                     _ -> error $ "Listener not found on node: " ++ show nid
--               -- 할당된 노드가 없으면
--               AssignSelf -> do
--                 -- 로컬에서 액터를 생성(spawnLocal)
--                 --liftIO $ putStrLn $ "[Process@" ++ show current ++ "] Received AssignSelf message"
--                 newPid <- spawnLocal $ do
--                       self <- getSelfPid
--                       --liftIO $ putStrLn $ "[Process@" ++ show self ++ "] SpawnLocal"
--                       let (loc, store1) = newref initStore (Actor_Val self)
--                           env1 = extend_env self x loc env
--                       _ <- value_of_k body env1 End_Main_Thread_Cont store1 actors
--                       forever $ liftIO $ threadDelay maxBound
--                 apply_cont saved_cont (Actor_Val newPid) store actors
--               ,
--               matchAny $ \msg -> do
--                   liftIO $ putStrLn $ "[WARN] Unexpected message in apply_cont: " ++ show msg
--                   error "Unexpected message received in apply_cont"
--             ]

apply_cont (New_Cont saved_cont) val store actors = do
  current <- getSelfPid
  let Procedure _ x body env = expval_proc val
      mainNid = mainNode actors
  whereisRemoteAsync mainNid "nodeRegistry"
  pidReply <- expectTimeout 5000000
  case pidReply of
    Just (WhereIsReply "nodeRegistry" (Just registryPid)) -> do
      send registryPid (RequestNode current)
      let loop = receiveWait
            [ match $ \(msg :: NodeMessage) -> case msg of
                AssignNode nid -> do
                  whereisRemoteAsync nid "nodeListener"
                  m <- expectTimeout 5000000
                  case m of
                    Just (WhereIsReply "nodeListener" (Just listenerPid)) -> do
                      send listenerPid (StartActor (ActorBehavior x body env actors) current)
                      response <- expectTimeout 5000000
                      case response of
                        Just newPid -> apply_cont saved_cont (Actor_Val newPid) store actors
                        Nothing     -> error "Did not receive PID from nodeListener"
                    _ -> error $ "Listener not found on node: " ++ show nid
                AssignSelf -> do
                  newPid <- spawnLocal $ do
                    self <- getSelfPid
                    let (loc, store1) = newref initStore (Actor_Val self)
                        env1 = extend_env self x loc env
                    _ <- value_of_k body env1 End_Main_Thread_Cont store1 actors
                    forever $ liftIO $ threadDelay maxBound
                  apply_cont saved_cont (Actor_Val newPid) store actors
            , matchAny $ \msg -> do
                liftIO $ putStrLn $ "[WARN] Unexpected message in apply_cont"
                loop  -- 다시 대기
            ]
      loop
    _ -> error "nodeRegistry not found on main node"

apply_cont (Actor1_Cont exp2 env cont) val1 store actors =
  value_of_k exp2 env (Actor2_Cont val1 cont) store actors

apply_cont (Actor2_Cont val1 cont) val2 store actors =
  let id = expval_actor val1
      id' = expval_actor val2    
  in  if (id == id')
      then apply_cont cont (Bool_Val True) store actors
      else apply_cont cont (Bool_Val False) store actors

apply_cont (Tuple_Cont explist vals env saved_cont) val store actors = do
  let vals' = vals ++ [val] in
    case explist of
      (exp:exps) -> value_of_k exp env (Tuple_Cont exps vals' env saved_cont) store actors
      [] -> let tuple = List_Val vals' 
            in apply_cont saved_cont tuple store actors

apply_cont (Let_Tuple_Cont vars body env saved_cont) val store actors = do
  current <- getSelfPid
  case val of
    List_Val vals -> 
      if vars == [] && null vals
      then value_of_k body env saved_cont store actors
      else let (env', store') = bind_vars current vars vals env store
           in value_of_k body env' saved_cont store' actors
    _ -> error ("LetTuple_Cont: expected a list, got " ++ show val)

apply_cont (Append_Cont var env cont) val store actors = do
  let (loc, store1) = apply_env env store var
      List_Val xs = deref store1 loc
      xs' = xs ++ [val]
      store2 = setref store1 loc (List_Val xs')
  apply_cont cont (Unit_Val) store2 actors



-- Todo: Introduce exceptions and define apply_handler to see how complex it is!
-- Todo: Use the monadic style to hide as many global parameters as possible.

apply_unop :: UnaryOp -> ExpVal -> Process ExpVal

apply_unop IsZero (Num_Val num)
  | num==0    = return $ Bool_Val True
  | otherwise = return $ Bool_Val False
apply_unop IsNull (List_Val [])  = return $ Bool_Val True
apply_unop IsNull (List_Val _)   = return $ Bool_Val False
apply_unop Car (List_Val (x:_))  = return $ x
apply_unop Cdr (List_Val (_:xs)) = return $ List_Val xs
apply_unop Print v = do
  liftIO $ putStr (show v)
  return Unit_Val
apply_unop Read _ = do
  -- liftIO $ putStrLn ""
  -- liftIO $ threadDelay 100000
  -- liftIO $ putStr ">> "
  liftIO $ hFlush stdout
  line <- liftIO getLine
  -- 입력 받은 직후, 이전 줄 지우기(ANSI escape code)
  -- 커서를 한 줄 위로 올리고, 줄 전체 지우기
  liftIO $ putStr "\ESC[A"    -- 커서 위로 한 줄
  liftIO $ putStr "\ESC[2K"   -- 줄 전체 지우기
  liftIO $ hFlush stdout
  return $ String_Val line
apply_unop op rand = error ("Unknown unary operator: :" ++ show op ++ " " ++ show rand)




value_of_k :: Exp -> Env -> Cont -> Store -> ActorState -> Process (FinalAnswer, Store)

value_of_k (Const_Exp n) env cont store actors = do
  apply_cont cont (Num_Val n) store actors

value_of_k (Const_List_Exp nums) env cont store actors = do
  apply_cont cont (List_Val (map Num_Val nums)) store actors

value_of_k (Str_Exp str) env cont store actors = do
  let strVal = String_Val str
  apply_cont cont strVal store actors

value_of_k (Pid_Exp pid) env cont store actors = do
  let pidVal = Actor_Val pid
  apply_cont cont pidVal store actors

value_of_k (Var_Exp var) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Var_Exp var)"
  let saved_actor = lookup_env env var
  if saved_actor == current
  then do
    let (loc, store') = apply_env env store var
        val = deref store' loc
    apply_cont cont val store' actors
  else do
    send saved_actor (RemoteVar (Var_Exp var, env) current)
    apply_cont (RemoteReady_Cont cont) (Proc_Val (procedure current "$dummy" (Const_Exp 42) env)) store actors

value_of_k (Diff_Exp exp1 exp2) env cont store actors =
  value_of_k exp1 env (Diff1_Cont exp2 env cont) store actors

value_of_k (Unary_Exp op exp1) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Unary_Exp op exp1), Op: " ++ show op
  value_of_k exp1 env (Unop_Arg_Cont op cont) store actors
  
value_of_k (If_Exp exp1 exp2 exp3) env cont store actors =
  value_of_k exp1 env (If_Test_Cont exp2 exp3 env cont) store actors

value_of_k (Let_Exp var exp1 body) env cont store actors = do
  value_of_k exp1 env (Let_Exp_Cont var body env cont) store actors

value_of_k (Letrec_Exp nameActorNameArgBodyList letrec_body) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Letrec_Exp nameActorNameArgBodyList letrec_body) "
  let nameActorIdArgBodyList =
        [ case maybeActorName of
            Nothing -> (p_name, current, b_var, p_body)
            Just actorName ->
              let (loc, store1) = apply_env env store actorName   -- Ignore store1!!
                  actorId  = expval_actor(deref store1 loc)
              in (p_name,actorId,b_var,p_body)
          | (p_name,maybeActorName,b_var,p_body) <- nameActorNameArgBodyList ]
  value_of_k letrec_body (extend_env_rec nameActorIdArgBodyList env) cont store actors

value_of_k (Proc_Exp (Just actorName) var body) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Proc_Exp (Just actorName) var body) "
  let (loc, store1) = apply_env env store actorName
      actorId = expval_actor (deref store1 loc)
  if actorId == current
  then apply_cont cont (Proc_Val (procedure current var body env)) store actors
  else do
    send actorId (RemoteProc (Proc_Exp Nothing var body) current)
    apply_cont (RemoteReady_Cont cont) (Proc_Val (procedure current "$dummy" (Const_Exp 42) env)) store actors

value_of_k (Proc_Exp Nothing var body) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Proc_Exp Nothing var body) "
  apply_cont cont (Proc_Val (procedure current var body env)) store actors

value_of_k (Call_Exp rator rand) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Call_Exp rator rand) "
  value_of_k rator env (Rator_Cont rand env cont) store actors
  
value_of_k (Block_Exp [exp]) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Block_Exp [exp]) "
  value_of_k exp env cont store actors

value_of_k (Block_Exp (exp:exps)) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Block_Exp (exp:exps)) "
  value_of_k (Call_Exp (Proc_Exp Nothing "$dummy" (Block_Exp exps)) exp) env cont store actors

value_of_k (Block_Exp []) env cont store actors =
  error "Unexpected empty block" 

value_of_k (Set_Exp x exp) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Set_Exp x exp) "
  value_of_k exp env (Set_Rhs_Cont x env cont) store actors

value_of_k (Spawn_Exp exp) env cont store actors =
  value_of_k exp env (Spawn_Cont cont) store actors 

-- value_of_k Yield_Exp env cont store actors =
--   let yiel =
--         place_on_ready_queue
--           (apply_cont cont (Num_Val 99))
--          
--   in  run_next_actor store yiel actors  -- run_next_thread

-- value_of_k Mutex_Exp env cont store actors =
--   let (mutex, store') = new_mutex store in
--     apply_cont cont (Mutex_Val mutex) store' actors 

-- value_of_k (Wait_Exp exp) env cont store actors =
--   value_of_k exp env (Wait_Cont cont) store actors 

-- value_of_k (Signal_Exp exp) env cont store actors =
--   value_of_k exp env (Signal_Cont cont) store actors 

-- For actors
value_of_k (Send_Exp (exp:exps)) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Send_Exp (exp:exps)) "
  value_of_k exp env (Send_Cont exps [] env cont) store actors

value_of_k (Ready_Exp exp) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (Ready_Exp exp) "
  value_of_k exp env (Ready_Cont cont) store actors

value_of_k (New_Exp exp) env cont store actors = do
  current <- getSelfPid
  --liftIO $ putStrLn $ "[value_of_k@" ++ show current ++ "] (New_Exp exp) "
  value_of_k exp env (New_Cont cont) store actors

value_of_k (Eq_Actor_Exp exp1 exp2) env cont store actors =
  value_of_k exp1 env (Actor1_Cont exp2 env cont) store actors

-- For tuple
value_of_k (Tuple_Exp []) env cont store actors = do
  apply_cont cont (List_Val []) store actors

value_of_k (Tuple_Exp (exp:exps)) env cont store actors = do
  value_of_k exp env (Tuple_Cont exps [] env cont) store actors

value_of_k (LetTuple_Exp vars exp1 exp2) env cont store actors = do
  value_of_k exp1 env (Let_Tuple_Cont vars exp2 env cont) store actors

value_of_k (Append_Exp exp1 exp2) env cont store actors = do
  value_of_k exp2 env (Append_Cont exp1 env cont) store actors

value_of_k (Log_Exp str exp) env cont store actors = do
  current <- getSelfPid
  trace ("[actor" ++ show current ++"]") $
    trace str $ 
      value_of_k exp env cont store actors

value_of_k exp _ _ _ _ =
  error $ "Unknown expression in value_of_k" ++ show exp


--
value_of_program :: Exp -> Process FinalAnswer
value_of_program exp = do
  nid <- getSelfNode
  -- let (loc, store1) = newref initStore (Actor_Val 0)
  --     env1 = extend_env 0 "main" loc initEnv
  (finalVal, _ ) <- value_of_k exp initEnv End_Main_Thread_Cont initStore (initActorState nid)
  return finalVal

--
initEnv = empty_env

--
apply_procedure_k :: Proc -> ExpVal -> Cont -> Store -> ActorState -> Process (FinalAnswer, Store)
apply_procedure_k proc arg cont store actors = do
  let (loc,store') = newref store arg
  value_of_k (body proc) (extend_env (saved_actor proc) (var proc) loc (saved_env proc)) cont store' actors
