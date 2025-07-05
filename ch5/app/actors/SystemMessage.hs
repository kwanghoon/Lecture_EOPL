{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module SystemMessage (SystemMessage(..)) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Distributed.Process (ProcessId)

data SystemMessage =
  CONNECT String ProcessId
  deriving (Generic, Binary, Typeable)
