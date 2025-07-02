{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Expr(Program,Exp(..),Identifier,UnaryOp(..),CompOp(..)) where

import ActorName(ActorName, RoleName)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Control.Distributed.Process(ProcessId)

type Program = Exp


data Exp =
    Const_Exp  Int
  | Str_Exp    String
  | Pid_Exp    ProcessId
  | Diff_Exp   Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier                 -- variable : x
  | Let_Exp    Identifier Exp Exp         -- let x = expression in expression
  | Letrec_Exp
      [(Identifier, Maybe ActorName, Identifier,Exp)] Exp   -- letrec { ..., f_i actorName (x_i) = expression_i, ... } in expression
  | Proc_Exp   (Maybe ActorName) Identifier Exp             -- proc actorName ( identifier ) expression
  | ProcAt_Exp RoleName Exp               -- proc @roleName ( identifier ) expression
  | Call_Exp   Exp Exp                    -- ( expression expression )
  | Block_Exp  [ Exp ]                    -- begin exp1; ...; expk end
  | Set_Exp    Identifier Exp             -- set x = expression
  | Const_List_Exp  [Int]                 -- number list : [ number1, ..., numberk ]
  | Unary_Exp  UnaryOp Exp                -- unop ( expression ) where unop is one of car, cdr, null?, zero? print
  | Comp_Exp   CompOp Exp Exp             -- ( expression compOp expression )

  -- For Actors
  | Send_Exp     [ Exp ]                  -- send ( to , msgs ) -> send ( SendExpressionList )
  | Ready_Exp    Exp                      -- ready ( expression ) 
  | New_Exp      Exp                      -- new ( expression )
  | Spawn_Exp  Exp                        -- spawn ( expression )

  -- For Tuple
  | Tuple_Exp    [ Exp ]                  -- ( expression1, ..., expressionk )
  | LetTuple_Exp [ Identifier ] Exp Exp   -- let x1, ..., xk = expression in expression
  | Append_Exp   Identifier Exp           -- append ( var , expression )

  deriving (Show, Generic, Binary)


data UnaryOp = IsZero | IsNull | Car | Cdr | Print | Read
  deriving (Show, Generic, Binary)

data CompOp = Eq                -- more comparison operators can be added here
  deriving (Show, Generic, Binary, Eq)

type Identifier = String
