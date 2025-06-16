{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module NodeRegistry
  ( NodeMessage(..)
  , NodeRegistry
  , newRegistry
  , registerNode
  , assignNode
  , removeNode
  ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Distributed.Process
import Data.Binary
import Data.Typeable
import GHC.Generics
import Control.Distributed.Process.Serializable

-- Messages
data NodeMessage
  = RegisterNode NodeId
  | RequestNode ProcessId
  | AssignNode NodeId
  | AssignSelf
  deriving (Show, Typeable, Generic)

instance Binary NodeMessage

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
      writeTVar registry (rest ++ [nid])
      return (Just nid)
    [] -> return Nothing

removeNode :: NodeId -> NodeRegistry -> STM ()
removeNode nid registry = do
  nids <- readTVar registry
  writeTVar registry (filter (/= nid) nids)
