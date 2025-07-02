{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module NodeRegistry
  ( NodeMessage(..)
  , NodeRegistry
  , newRegistry
  , registerNode
  , assignNode
  , removeNode
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Distributed.Process (ProcessId, NodeId)

-- Messages
data NodeMessage
  = RegisterNode NodeId
  | RequestNode ProcessId
  | AssignNode NodeId
  | AssignSelf
  deriving (Generic, Binary, Typeable)

type NodeRegistry = TVar [NodeId]

-- Registry functions
newRegistry :: IO NodeRegistry
newRegistry = newTVarIO []

registerNode :: NodeId -> NodeRegistry -> STM ()
registerNode nid registry = do
  nids <- readTVar registry
  if nid `elem` nids
  then return ()
  else writeTVar registry (nid : nids)

assignNode :: NodeRegistry -> STM (Maybe NodeId)
assignNode registry = do
  nids <- readTVar registry
  case nids of
    (nid:rest) -> do
      writeTVar registry (rest)
      return (Just nid)
    [] -> return Nothing

removeNode :: NodeId -> NodeRegistry -> STM ()
removeNode nid registry = do
  nids <- readTVar registry
  writeTVar registry (filter (/= nid) nids)