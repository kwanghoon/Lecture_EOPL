{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import DynamicMessage

import System.IO
import System.Environment (getArgs)
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Control.Concurrent.STM

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)
import Network.Transport (EndPointAddress(..))
import qualified Data.ByteString.Char8 as BS

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable
import Data.Char (toLower)
import Data.List (isPrefixOf, isInfixOf)


-- Entrypoint :
--    stack run actors-exe 시 전달받는 인자에 따라
--    main 노드 또는 원격 노드를 실행
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("main":addrStr:fileName:_)     -> runMainNode addrStr fileName
    ("node":addrStr:mainAddrStr:_)  -> runRemoteNode addrStr mainAddrStr
    ("dynamic-node":addrStr:mainAddrStr:_) -> runDynamicNode addrStr mainAddrStr
    _                               -> putStrLn "Usage:\n  actors-exe main <ip:port> <file>\n actors-exe node <ip:port> <ip:port>\n actors-exe dynamic-node <ip:port> <ip:port>"


-- Main Node :
--   Background listener (process) : 원격 노드 등록 및 관리
--   Run interpreter (process)     : 예제 파일 실행
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

  mvar <- newEmptyMVar              -- just for prompt

  -- Background 1 : 정적 노드 레지스트리 프로세스
  _ <- forkIO $ runProcess node $ do
    mNid <- getSelfNode
    liftIO $ putMVar mvar mNid      -- just for prompt

    -- "nodeRegistry"라는 이름으로 현재 프로세스 등록
    self <- getSelfPid
    register "nodeRegistry" self
    liftIO $ putStrLn $ "[Backgournd1@" ++ show mNid ++ "] Node registry process started."

    -- 무한 루프 : 
    --    1. 노드 등록 (stack run actors-exe node ...)
    --    2. 할당 요청 (New_Exp)
    --    3. 노드 다운 모니터링
    forever $ do
      receiveWait
        [ match $ \(msg :: NodeMessage) -> 
            case msg of
              -- 새 노드 등록 요청 처리
              RegisterNode nid -> do
                _ <- monitorNode nid
                liftIO $ atomically $ registerNode nid nodesRegistry
                liftIO $ do
                  putStrLn $ "\n[Background1@" ++ show mNid ++ "] Registered node: " ++ show nid
                  printPrompt mNid

              -- 노드 할당 요청 처리
              RequestNode requester -> do
                nids <- liftIO $ atomically $ assignNode nodesRegistry
                case nids of
                  Just nid -> do
                    send requester (AssignNode nid)
                    liftIO $ putStrLn $ "[Background1@" ++ show mNid ++ "] Assigned node: " ++ show nid
                  Nothing -> do
                    send requester AssignSelf
                    liftIO $ putStrLn $ "[Background1@" ++ show mNid ++ "] No available node"
          ,
          -- 다운된 노드 감지 시 레지스트리에서 제거
          match $ \(NodeMonitorNotification reason downedNode _) -> do
            liftIO $ atomically $ removeNode downedNode nodesRegistry
            liftIO $ putStrLn $ "\n[Background1@" ++ show mNid ++ "] Node down: " ++ show downedNode ++ " removed"
        ]

  mNid <- takeMVar mvar       -- just for prompt

  -- Background 2 : 동적 노드 레지스트리 프로세스
  _ <- forkIO $ runProcess node $ do
    self <- getSelfPid
    nid <- getSelfNode
    register "dynamicNodeRegistry" self

    liftIO $ putStrLn $ "[Background2@" ++ show nid ++ "] Starting dynamic node"

    -- 파일에서 behavior 이름 추출
    text <- liftIO $ readFile fileName
    let behaviorNames = extractBehaviorNames text  -- e.g., ["serverBehavior", "clientBehavior"]
    
    -- 초기 저장소: main 노드가 자신 pid를 "main"에 바인딩
    pidMapVar <- liftIO $ atomically $ newTVar [("main", self)]

    -- 메시지 수신 루프:
    forever $ receiveWait
      [ match (\(msg :: DynamicMessage) -> case msg of
          RequestBehaviorList from -> 
            send from (BehaviorList behaviorNames)
          RequestPidMap from -> do
            currentMap <- liftIO $ atomically $ readTVar pidMapVar
            send from (RespondPidMap currentMap)
          RegisterNamedPid name pid -> do
            liftIO $ atomically $ modifyTVar' pidMapVar (\m -> (name, pid) : filter ((/= name) . fst) m)
            liftIO $ putStrLn $ "[Background2@" ++ show nid ++ "] Registered: " ++ name ++ " for " ++ show pid )
      ]

  -- wait for user command (start or status)
  putStrLn $ "[Main@" ++ show mNid ++ "] Waiting for command ..."
  waitForStartCommand mNid nodesRegistry


  -- Run the interpreter (by start command)
  runProcess node $ do
    pid <- getSelfPid
    register "dynamicMainInterp" pid
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

    result <- value_of_program expression
    liftIO $ putStrLn ("[Main@" ++ show pid ++ "] Final result: " ++ show result)

    forever $ liftIO $ threadDelay maxBound

waitForStartCommand :: NodeId -> NodeRegistry -> IO ()
waitForStartCommand mNid nodesRegistry = do
  printPrompt mNid
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

printPrompt :: NodeId -> IO ()
printPrompt mNid = do
  putStrLn $ "[Main@" ++ show mNid ++ "] Type 'start' or 'status'"
  hFlush stdout

-- e.g. "let serverBehavior = proc(self) ..." 형태 탐지
extractBehaviorNames :: String -> [String]
extractBehaviorNames = map extractName . filter isBehaviorDef . lines
  where
    isBehaviorDef line = "let " `isPrefixOf` line && "Behavior = proc" `isInfixOf` line
    extractName line = takeWhile (/= ' ') . drop 4 $ line  -- skip "let "



-- Remote node :
--  메인 노드에 자신의 존재를 등록하고 작업 대기
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
          send requester pid
      ]



runDynamicNode :: String -> String -> IO ()
runDynamicNode addrStr mainAddrStr = do
  let (host, portStr)         = break (== ':') addrStr
      (mainHost, mainPortStr) = break (== ':') mainAddrStr
  Right transport <- createTransport (defaultTCPAddr host (tail portStr)) defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  runProcess node $ do
    self <- getSelfPid
    let mainNodeId = NodeId (EndPointAddress (BS.pack (mainAddrStr ++ ":0")))

    -- main 노드의 registry 찾기
    whereisRemoteAsync mainNodeId "dynamicNodeRegistry"
    reply <- expect
    (registryPid, behaviors) <- case reply of
      (WhereIsReply "dynamicNodeRegistry" (Just pid)) -> do
        send pid (RequestBehaviorList self)
        BehaviorList behaviors <- expect
        return (pid, behaviors)
      (WhereIsReply "dynamicNodeRegistry" Nothing) -> do
        liftIO $ putStrLn "Registry not found on main node!"
        error "Registry lookup failed"
      _ -> do
        liftIO $ putStrLn $ "Unexpected reply: " ++ show reply
        error "Unexpected reply to whereisRemoteAsync"

    -- 선택 요청
    (behaviorName, actorVar) <- liftIO $ promptForBehaviorAndVar behaviors

    -- 선택 후 서버에 등록 요청 및 바인딩 요청
    send registryPid (RegisterNamedPid actorVar self)

    -- 최신 바인딩 맵을 받음
    send registryPid (RequestPidMap self)
    RespondPidMap pidMap <- expect

    -- main 노드의 interpreter에 env, store 요청
    whereisRemoteAsync mainNodeId "dynamicMainInterp"
    reply <- expect
    (env, store) <- case reply of
      (WhereIsReply "dynamicMainInterp" (Just pid)) -> do
        send pid (RequestEnvStore self)
        RespondEnvStore (env, store) <- expect
        return (env, store)
      (WhereIsReply "dynamicMainInterp" Nothing) -> do
        liftIO $ putStrLn "Registry not found on main node!"
        error "Registry lookup failed"
      _ -> do
        liftIO $ putStrLn $ "Unexpected reply: " ++ show reply
        error "Unexpected reply to whereisRemoteAsync"

    let (loc, store1) = apply_env env store behaviorName
        proc = deref store1 loc
        savedVar = var (expval_proc proc)
        savedBody = body (expval_proc proc)
        savedEnv = saved_env (expval_proc proc)

    -- 여러 (이름, PID) 바인딩을 누적해서 전체 Exp 조립
    let allPidBindings = pidMap ++ [(savedVar, self)]
        exp = buildPidBindings allPidBindings savedBody
    
    _ <- value_of_k exp savedEnv End_Main_Thread_Cont initStore (initActorState mainNodeId)

    liftIO $ putStrLn ("[Process@" ++ show self ++ "] Done.")
    forever $ liftIO $ threadDelay maxBound


promptForBehaviorAndVar :: [String] -> IO (String, String)
promptForBehaviorAndVar available = do
  putStrLn "\nAvailable behaviors:"
  mapM_ putStrLn available
  putStr "Enter behavior name exactly as listed: "
  hFlush stdout
  behaviorName <- getLine
  if behaviorName `notElem` available
    then do
      putStrLn "Invalid behavior name. Please try again."
      promptForBehaviorAndVar available
    else do
      putStr "Enter variable name to bind this actor to : "
      hFlush stdout
      varName <- getLine
      if null varName
        then do
          putStrLn "Variable name cannot be empty. Try again."
          promptForBehaviorAndVar available
        else return (behaviorName, varName)

-- 여러 pid 바인딩을 중첩된 let으로 조립
buildPidBindings :: [(String, ProcessId)] -> Exp -> Exp
buildPidBindings [] body = body
buildPidBindings ((name, pid):rest) body =
  Let_Exp name (Pid_Exp pid) (buildPidBindings rest body)