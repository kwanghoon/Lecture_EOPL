{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CommonParserUtil
import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import EnvStore
import Interp
import NodeRegistry
import SystemMessage

import System.IO
import System.Environment (getArgs)
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, unless, forM_, when)
import Control.Concurrent.STM

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)
import Network.Transport (EndPointAddress(..))
import qualified Data.ByteString.Char8 as BS

import Data.Char (toLower)


-- Entrypoint :
--    stack run actors-exe 시 전달받는 인자에 따라
--    main 노드 또는 원격 노드를 실행
-- stack run actors-exe main <ip:port> <file>

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("main":addrStr:fileName:_)     -> runMainNode addrStr fileName
    ("node":addrStr:mainAddrStr:_)  -> runRemoteNode addrStr mainAddrStr
    (role:addrStr:mainAddrStr:_)    -> runNodeByRole role addrStr mainAddrStr
    _                               -> putStrLn "Usage:\n  actors-exe main <ip:port> <file>\n actors-exe node <ip:port> <ip:port>\n actors-exe role <ip:port> <ip:port>"


-- Main Node :
--    Background listener (process) : 원격 노드 등록 및 관리
--    Run interpreter (process)     : 파일 실행
runMainNode :: String -> FilePath -> IO ()
runMainNode addrStr fileName = do
  -- create TCP transport and Main node
  let (host, portStr) = break (== ':') addrStr
  eTransport <- createTransport (defaultTCPAddr host (tail portStr)) defaultTCPParameters
  node <- case eTransport of
    Right transport -> newLocalNode transport initRemoteTable
    Left err -> do
      putStrLn $ "Transport creation failed: " ++ show err
      error "Failed to create transport"

  -- 메인과 연결된 노드들 관리용 노드 레지스트리 생성 (STM TVar)
  nodesRegistry <- newRegistry
  rolesRegistry <- newRoleRegistry 

  mvar <- newEmptyMVar                      -- just for prompt
  interpRun <- atomically $ newTVar False   -- just for prompt

  -- Background listener : 노드 레지스트리 프로세스
  _ <- forkIO $ runProcess node $ do
    mNid <- getSelfNode
    liftIO $ putMVar mvar mNid      -- just for prompt

    -- "nodeRegistry"라는 이름으로 현재 프로세스 등록
    self <- getSelfPid
    register "nodeRegistry" self
    liftIO $ putStrLn $ "[Main@" ++ show mNid ++ "] Node registry process started."

    -- 무한 루프 : 
    --    1. 노드 등록 (stack run actors-exe node ...)
    --    2. 할당 요청 (New_Exp)
    --    3. 노드 다운 모니터링
    forever $ do
      receiveWait
        [ match $ \(msg :: NodeMessage) -> 
            case msg of
              -- 새 노드 등록 요청 처리
              RegisterNode requesterNid -> do
                _ <- monitorNode requesterNid
                liftIO $ atomically $ registerNode requesterNid nodesRegistry
                liftIO $ do
                  putStrLn $ "\n[Main@" ++ show mNid ++ "] Registered node: " ++ show requesterNid
                  printPrompt interpRun mNid

              -- 액터 생성을 담당할 노드 선택
              -- case 1 : main 노드만 있는 경우, main 노드에 액터 생성
              -- case 2 : main 노드와 하나 이상의 원격 노드가 있는 경우, 원격 노드 중 랜덤하게 선택하여 액터 생성
              RequestNode requesterPid -> do
                nids <- liftIO $ atomically $ assignNode nodesRegistry
                case nids of
                  Just nid -> do
                    send requesterPid (AssignNode nid)
                    liftIO $ putStrLn $ "[Main@" ++ show mNid ++ "] Assigned node: " ++ show nid
                  Nothing -> do
                    send requesterPid AssignSelf
                    liftIO $ putStrLn $ "[Main@" ++ show mNid ++ "] No available node"

              -- role을 지정하여 새 노드 등록 요청 처리
              RegisterRole role requesterPid -> do
                _ <- monitor requesterPid
                liftIO $ atomically $ registerRole rolesRegistry role requesterPid
                allPids <- liftIO $ atomically $ getAllPids rolesRegistry
                forM_ allPids $ \pid ->
                  when (pid /= requesterPid) $ send pid (CONNECT role requesterPid)
                liftIO $ putStrLn $ "\n[Main@" ++ show mNid ++ "] Registered role: " ++ show requesterPid
              
              -- @roleName 을 가진 프로세스 조회 요청 처리
              RequestRole role requesterPid -> do
                pids <- liftIO $ atomically $ getPidByRoles role rolesRegistry
                case pids of
                  [] -> send requesterPid NotFound
                  _  -> send requesterPid (RoleFound pids)
          ,
          -- 다운된 노드 감지 시 레지스트리에서 제거
          match $ \(NodeMonitorNotification _ downedNode _) -> do
            liftIO $ atomically $ removeNode downedNode nodesRegistry
            liftIO $ putStrLn $ "\n[Main@" ++ show mNid ++ "] Node down: " ++ show downedNode ++ " removed"
          ,
          match $ \(ProcessMonitorNotification _ deadPid _) -> do
            liftIO $ atomically $ removeProcess rolesRegistry deadPid
            liftIO $ putStrLn $ "\n[Main@" ++ show mNid ++ "] Role down: " ++ show deadPid ++ " removed"
        ]

  mNid <- takeMVar mvar       -- just for prompt

  -- wait for user command (start or status)
  -- putStrLn $ "[Main@" ++ show mNid ++ "] Waiting for command ..."
  -- waitForStartCommand mNid nodesRegistry

  -- Run the interpreter (by start command)
  runProcess node $ do
    pid <- getSelfPid
    register "mainInterp" pid   -- main 액터의 pid 이름
    liftIO $ atomically $ registerRole rolesRegistry "main" pid
    liftIO $ putStrLn fileName
    text <- liftIO $ readFile fileName

    let debugFlag = False
    pet <- liftIO $
      parsing debugFlag
        parserSpec ((), 1, 1, text)
        (aLexer lexerSpec)
        (fromToken (endOfToken lexerSpec))
    
    let expression = expFrom pet
    liftIO $ print expression

    liftIO $ atomically $ writeTVar interpRun True   -- just for prompt

    result <- value_of_program expression
    -- runReadyServiceLoop store (initActorState mNid)
    liftIO $ putStrLn ("[Main@" ++ show pid ++ "] Final result: " ++ show result)

    forever $ liftIO $ threadDelay maxBound

waitForStartCommand :: NodeId -> NodeRegistry -> IO ()
waitForStartCommand mNid nodesRegistry = do
  putStrLn $ "[Main@" ++ show mNid ++ "] Type 'start' or 'status'"
  hFlush stdout
  line <- getLine
  case map toLower line of
    "start" -> putStrLn $ "[Main@" ++ show mNid ++ "] Starting interpreter..."
    "status" -> do
      nodeCount <- atomically $ length <$> readTVar nodesRegistry
      putStrLn $ "[Main@" ++ show mNid ++ "] Currently registered nodes: " ++ show nodeCount
      waitForStartCommand mNid nodesRegistry
    _ -> do
      putStrLn $ "[Main@" ++ show mNid ++ "] Unknown command. Try again."
      waitForStartCommand mNid nodesRegistry

printPrompt :: TVar Bool -> NodeId -> IO ()
printPrompt runningFlag mNid = do
  isRunning <- atomically $ readTVar runningFlag
  unless isRunning $ do
    putStrLn $ "[Main@" ++ show mNid ++ "] Type 'start' or 'status'"
    hFlush stdout


-- Remote node :
-- stack run actors-exe node <ip:port> <ip:port>
-- 메인 노드에 자신의 존재를 등록하고 작업 대기
runRemoteNode :: String -> String -> IO ()
runRemoteNode addrStr mainAddrStr = do
  let (host, portStr)         = break (== ':') addrStr
      (mainHost, mainPortStr) = break (== ':') mainAddrStr
  Right transport <- createTransport (defaultTCPAddr host (tail portStr)) defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  runProcess node $ do
    myNode <- getSelfNode
    liftIO $ putStrLn ("[Node@" ++ show myNode ++ "] Registering with main node.")
    let mainNodeId = NodeId (EndPointAddress (BS.pack $ mainAddrStr ++ ":0"))   -- 메인 노드 주소로 NodeId 생성
    
    -- 메인 노드에 nodeRegistry 프로세스 위치 요청
    whereisRemoteAsync mainNodeId "nodeRegistry"
    receiveWait
      [ match $ \(WhereIsReply "nodeRegistry" mPid) ->
          case mPid of
            Just pid -> do
              -- 메인 노드에 자신의 NodeId 등록 요청
              send pid (RegisterNode myNode)
              liftIO $ putStrLn ("[Node@" ++ show myNode ++ "] Successfully registered.")
            Nothing  -> 
              liftIO $ putStrLn ("[Node@" ++ show myNode ++ "] nodeRegistry not found.")
      ]
    
    -- 노드 내 작업 수신 대기 프로세스 생성
    _ <- spawnLocal nodeListener
    liftIO $ putStrLn ("[Node@" ++ show myNode ++ "] nodeListener started and waiting...")

    -- 무한 대기 (프로세스 종료 방지)
    forever $ liftIO $ threadDelay maxBound

-- 원격 노드 내에서 메시지 대기 및 액터 시작 요청 처리하는 프로세스
nodeListener :: Process ()
nodeListener = do
  self <- getSelfPid
  register "nodeListener" self
  liftIO $ putStrLn $ "[Process@" ++ show self ++ "] nodeListener started."
  forever $ do
    receiveWait
      [ match $ \(StartActor (ActorBehavior x body env actors) requester) -> do
          liftIO $ putStrLn $ "[Process@" ++ show self ++ "] Received StartActor message"
          pid <- spawnLocal ( do
              self <- getSelfPid
              liftIO $ putStrLn $ "[Process@" ++ show self ++ "] SpawnLocal"
              let (loc, store1) = newref initStore (Actor_Val self)
                  env1 = extend_env self x loc env
              _ <- value_of_k body env1 End_Main_Thread_Cont store1 actors
              forever $ liftIO $ threadDelay maxBound )
          whereisRemoteAsync (mainNode actors) "nodeRegistry"
          receiveWait
            [ match $ \(WhereIsReply "nodeRegistry" (Just mPid)) -> 
                send mPid (RegisterRole "node" pid)
            ]
          send requester pid
      ]


-- Remote node with Role :
-- stack run actors-exe role <ip:port> <ip:port>
--    메인 노드에 자신의 존재를 등록하고 
--    인터프리터로 readyRoleExp를 실행시켜 Role에 해당하는 동작 받음
runNodeByRole :: String -> String -> String -> IO ()
runNodeByRole role addrStr mainAddrStr = do
  let (host, portStr) = break (== ':') addrStr
  Right transport <- createTransport (defaultTCPAddr host (tail portStr)) defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  runProcess node $ do
    self <- getSelfPid
    myNode <- getSelfNode
    liftIO $ putStrLn ("[Node@" ++ show myNode ++ "] Registering with main node.")
    let mainNodeId = NodeId (EndPointAddress (BS.pack $ mainAddrStr ++ ":0"))   -- 메인 노드 주소로 NodeId 생성
    
    -- 메인 노드에 nodeRegistry 프로세스 위치 요청
    whereisRemoteAsync mainNodeId "nodeRegistry"
    receiveWait
      [ match $ \(WhereIsReply "nodeRegistry" mPid) ->
          case mPid of
            Just pid -> do
              -- 메인 노드에 자신의 NodeId 등록 요청
              send pid (RegisterRole role self)
              liftIO $ putStrLn ("[Node@" ++ show myNode ++ "] Successfully registered.")
            Nothing  -> 
              liftIO $ putStrLn ("[Node@" ++ show myNode ++ "] nodeRegistry not found.")
      ]
    
    whereisRemoteAsync mainNodeId "mainInterp"
    receiveWait
      [ match $ \(WhereIsReply "mainInterp" mPid) ->
          case mPid of
            Just pid -> do
              (_,store) <- value_of_k readyExp initEnv End_Main_Thread_Cont initStore (initActorState mainNodeId)
              forever $ liftIO $ threadDelay maxBound -- todo : 삭제 여부 확인
            Nothing  -> do
              liftIO $ putStrLn ("[Node@" ++ show myNode ++ "] mainInterp not found.")
              error "mainInterp not found"
        ]

readyExp :: Exp
readyExp = Ready_Exp (Proc_Exp Nothing "d" (Var_Exp "d"))

-- Remote 메시지 처리 서비스 루프
runReadyServiceLoop :: Store -> ActorState -> Process ()
runReadyServiceLoop store actors = do
  store' <- runReadyService store actors
  runReadyServiceLoop store' actors

-- RemoteMessage 한 번 처리하고 업데이트된 Store 반환
runReadyService :: Store -> ActorState -> Process Store
runReadyService store actors = do
  liftIO $ putStrLn $ "runReadyService"
  current <- getSelfPid
  receiveWait
    [ match $ \(msg :: RemoteMessage) -> case msg of
        RemoteVar varLoc requester -> do
          let returnVal = deref store varLoc
          send requester (ReturnMessage returnVal)
          return store
        RemoteSet (varLoc, val') requester -> do
          let store1 = setref store varLoc val'
          send requester (ReturnMessage Unit_Val)
          return store1
        RemoteProc (Proc_Exp _ var body) savedEnv requester -> do
          -- make Proc_Val from the Proc_Exp
          (procVal, store1) <- value_of_k (Proc_Exp Nothing var body) savedEnv End_Main_Thread_Cont store actors
          let (loc, store2) = newref store1 procVal
          send requester (ReturnMessage (Loc_Val (remoteLocation loc current)))
          return store2
        RemoteCall (ratorLoc, randVal) requester -> do
          let procVal = deref store ratorLoc
              proc = expval_proc procVal
          (returnVal, store1) <- apply_procedure_k proc randVal End_Main_Thread_Cont store actors
          send requester (ReturnMessage returnVal)
          return store1
    ]