{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module NodeRegistry
  ( NodeMessage(..)
  , RoleRegistry
  , newRoleRegistry
  , registerRole
  , getPidByRoles
  , removeProcess
  , getAllPids
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Distributed.Process (ProcessId, NodeId)

-- Messages
data NodeMessage
  = RegisterRole String ProcessId
  | RequestRole String ProcessId
  | RoleFound [ProcessId]
  | NotFound
  deriving (Generic, Binary, Typeable)


-- role Registry
type Role = String
type RoleRegistry = TVar (Map Role [ProcessId])

newRoleRegistry :: IO RoleRegistry
newRoleRegistry = newTVarIO Map.empty

registerRole :: RoleRegistry -> Role -> ProcessId -> STM ()
registerRole registry role pid = do
  table <- readTVar registry
  let updated = Map.insertWith (++) role [pid] table
  writeTVar registry updated

getPidByRoles :: Role -> RoleRegistry -> STM [ProcessId]
getPidByRoles role registry = do
  table <- readTVar registry
  return $ Map.findWithDefault [] role table

removeProcess :: RoleRegistry -> ProcessId -> STM ()
removeProcess registry pid = do
  table <- readTVar registry
  let updated = Map.map (filter (/= pid)) table
  writeTVar registry updated

getAllPids :: RoleRegistry -> STM [ProcessId]
getAllPids registry = do
  table <- readTVar registry
  return $ concat (Map.elems table)