{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DynamicMessage where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import GHC.Generics
import Data.Binary
import Data.Typeable
import EnvStore (Env, Store)

data DynamicMessage
  = RequestBehaviorList ProcessId
  | BehaviorList [String]
  | RequestPidMap ProcessId
  | RespondPidMap [(String, ProcessId)]
  | RegisterNamedPid String ProcessId
  | RequestEnvStore ProcessId
  | RespondEnvStore (Env, Store)
  deriving (Generic, Typeable)

instance Binary DynamicMessage