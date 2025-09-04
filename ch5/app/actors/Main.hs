{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

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
import qualified Data.Map as Map


-- Entrypoint :
--    stack run actors-exe 시 전달받는 인자에 따라
--    main 노드 또는 원격 노드를 실행
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("main":addrStr:fileName:_)        -> runMainNode addrStr fileName
    (role:addrStr:mainAddrStr:fileName:_)   -> runNodeByRole role addrStr mainAddrStr fileName
    _ -> putStrLn $ unlines
      [ "Usage:"
      , "  actors-exe main <ip:port> <file>"
      , "  actors-exe <role> <ip:port> <main-ip:port> <file>"
      ]


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

  -- 메인과 연결된 프로세스들 레지스트리 생성 (STM TVar)
  rolesRegistry <- newRoleRegistry 

  mvar <- newEmptyMVar              -- just for prompt

  -- Background listener : 노드 레지스트리 프로세스
  _ <- forkIO $ runProcess node $ do
    mNid <- getSelfNode
    liftIO $ putMVar mvar mNid      -- just for prompt

    -- "nodeRegistry"라는 이름으로 현재 프로세스 등록
    self <- getSelfPid
    register "nodeRegistry" self
    liftIO $ putStrLn $ "[Main@" ++ show mNid ++ "] Node registry process started."

    -- 무한 루프 : 
    --    1. 노드 등록 (stack run actors-exe <role> ...)
    --    2. 노드 다운 모니터링
    forever $ do
      receiveWait
        [ match $ \(msg :: NodeMessage) -> 
            case msg of
              -- role을 지정하여 새 노드 등록 요청 처리
              RegisterRole role requesterPid -> do
                _ <- monitor requesterPid
                liftIO $ atomically $ registerRole rolesRegistry role requesterPid
                allPids <- liftIO $ atomically $ getAllPids rolesRegistry
                forM_ allPids $ \pid ->
                  when (pid /= requesterPid) $ send pid (List_Val [(Str_Val "CONNECT"), List_Val [(Str_Val role), (Actor_Val requesterPid)]])
                liftIO $ putStrLn $ "\n[Main@" ++ show mNid ++ "] Registered role: " ++ show requesterPid
          ,
          -- 다운된 노드 감지 시 레지스트리에서 제거
          match $ \(ProcessMonitorNotification _ deadPid _) -> do
            liftIO $ atomically $ removeProcess rolesRegistry deadPid
            liftIO $ putStrLn $ "\n[Main@" ++ show mNid ++ "] Role down: " ++ show deadPid ++ " removed"
        ]

  mNid <- takeMVar mvar

  -- Run the interpreter
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
        (_,procMap,transformedExp) = toProcMap expression 0 Map.empty
    liftIO $ putStrLn (show expression)
    (result,store) <- value_of_program transformedExp procMap
    liftIO $ putStrLn ("[Main@" ++ show pid ++ "] Final result: " ++ show result)
    runReadyServiceLoop store (initActorState mNid procMap)


-- Remote node with Role :
-- stack run actors-exe <role> <ip:port> <main-ip:port> <file>
--    메인 노드에 자신의 존재를 등록하고 (메인 노드가 접속된 프로세스들에게 브로드캐스트)
--    실행할 Behavior를 메시지로 받을 때까지 대기
runNodeByRole :: String -> String -> String -> FilePath -> IO ()
runNodeByRole role addrStr mainAddrStr fileName = do
  let (host, portStr) = break (== ':') addrStr
  Right transport <- createTransport (defaultTCPAddr host (tail portStr)) defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  runProcess node $ do
    self <- getSelfPid
    let mainNodeId = NodeId (EndPointAddress (BS.pack $ mainAddrStr ++ ":0"))   -- 메인 노드 주소로 NodeId 생성

    text <- liftIO $ readFile fileName

    let debugFlag = False
    pet <- liftIO $
      parsing debugFlag
        parserSpec ((), 1, 1, text)
        (aLexer lexerSpec)
        (fromToken (endOfToken lexerSpec))
    
    let expression = expFrom pet
        (_,procMap,_) = toProcMap expression 0 Map.empty

    -- 메인 노드에 nodeRegistry 프로세스 위치 요청
    whereisRemoteAsync mainNodeId "nodeRegistry"
    receiveWait
      [ match $ \(WhereIsReply "nodeRegistry" mPid) -> case mPid of
          Just pid -> do
            liftIO $ putStrLn ("[Process@" ++ show self ++ "] Registering with main node.")
            send pid (RegisterRole role self)
            let loop = receiveWait
                  [ match $ \(msg :: RemoteMessage) -> case msg of
                      RemoteProc idx savedEnv requester -> do
                        case Map.lookup idx procMap of
                          Just (Proc_Exp _ var body) -> do
                            let returnVal = Proc_Val (procedure self var body savedEnv)
                            send requester (ReturnMessage returnVal)
                            loop
                          Nothing -> error $ "Invalid RemoteProc index: " ++ show idx

                      RemoteCall (ratorVal, randVal) _ -> do
                        let proc = expval_proc ratorVal
                        (returnVal, store) <- apply_procedure_k proc randVal End_Main_Thread_Cont initStore (initActorState mainNodeId procMap)
                        runReadyServiceLoop store (initActorState mainNodeId procMap)
                  , matchAny $ \_ -> loop
                  ]
            loop
          Nothing  -> 
            liftIO $ putStrLn ("[Process@" ++ show self ++ "] nodeRegistry not found.")
      ]


-- Remote 메시지 처리 서비스 루프
runReadyServiceLoop :: Store -> ActorState -> Process ()
runReadyServiceLoop store actors = do
  store' <- runReadyService store actors
  runReadyServiceLoop store' actors

-- -- RemoteMessage 한 번 처리하고 업데이트된 Store 반환
runReadyService :: Store -> ActorState -> Process Store
runReadyService store actors = do
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

          RemoteProc idx savedEnv requester -> do
            let m = procMap actors
            case Map.lookup idx m of
              Just (Proc_Exp _ var body) -> do
                let returnVal = Proc_Val (procedure current var body savedEnv)
                send requester (ReturnMessage returnVal)
                return store
              Nothing -> error $ "Invalid RemoteProc index: " ++ show idx

          RemoteCall (ratorVal, randVal) requester -> do
            let proc = expval_proc ratorVal
            (returnVal, store1) <- apply_procedure_k proc randVal End_Main_Thread_Cont store actors
            send requester (ReturnMessage returnVal)
            return store1
    ]