{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Expr(Program,Exp(..),Identifier,UnaryOp(..),BinaryOp(..),CompOp(..),toProcMap) where

import ActorName(ActorName, RoleName)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Control.Distributed.Process(ProcessId)
import qualified Data.Map as Map

type Program = Exp


data Exp =
    Const_Exp  Int
  | Str_Exp    String
  | Pid_Exp    ProcessId
  -- | Diff_Exp   Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier                 -- variable : x
  | Let_Exp    Identifier Exp Exp         -- let x = expression in expression
  | Letrec_Exp
      [(Identifier, Maybe ActorName, Identifier,Exp)] Exp   -- letrec { ..., f_i actorName (x_i) = expression_i, ... } in expression
  | Proc_Exp   (Maybe ActorName) Identifier Exp             -- proc actorName ( identifier ) expression
  | Call_Exp   Exp Exp                    -- ( expression expression )
  | Block_Exp  [ Exp ]                    -- begin exp1; ...; expk end
  | Set_Exp    Identifier Exp             -- set x = expression
  | Const_List_Exp  [Int]                 -- number list : [ number1, ..., numberk ]
  | Unary_Exp  UnaryOp Exp                -- unop ( expression ) where unop is one of car, cdr, null?, zero? print
  | Binary_Exp BinaryOp Exp Exp
  | Comp_Exp   CompOp Exp Exp             -- ( expression compOp expression )

  -- For Actors
  | Send_Exp     [ Exp ]                  -- send ( to , msgs ) -> send ( SendExpressionList )
  | Ready_Exp    Exp                      -- ready ( expression ) 
  | New_Exp      Exp                      -- new ( expression )
  -- | Spawn_Exp  Exp                        -- spawn ( expression )

  -- For Tuple
  | Tuple_Exp    [ Exp ]                  -- ( expression1, ..., expressionk )
  | LetTuple_Exp [ Identifier ] Exp Exp   -- let x1, ..., xk = expression in expression
  | Append_Exp   Identifier Exp           -- append ( var , expression )
  | PtrTo_Exp Int 

  | PowMod_Exp Exp Exp Exp

  deriving (Show, Generic, Binary)


data UnaryOp = IsZero | IsNull | Car | Cdr | Print | Read | ReadInt
  deriving (Show, Generic, Binary)

data BinaryOp = Add | Diff | Mul | Mod | Random
  deriving (Show, Generic, Binary)

data CompOp = Eq                -- more comparison operators can be added here
  deriving (Show, Generic, Binary, Eq)

type Identifier = String

-- toProcMap
-- PtrTo_Exp가 없는 Exp를 PtrTo_Exp를 사용하는 Exp로 변환하는 함수
-- Proc_Exp를 만나면 Proc_Exp를 PtrTo_Exp로 변환하고, 그 Proc_Exp를 Map에 저장한다.
-- 이때 Map의 키는 Proc_Exp가 생성된 순서대로 증가하는 정수이다.
toProcMap :: Exp -> Int -> Map.Map Int Exp -> (Int, Map.Map Int Exp, Exp)
toProcMap (Const_Exp n) i m = (i, m, Const_Exp n)
toProcMap (Str_Exp s) i m = (i, m, Str_Exp s)
toProcMap (Pid_Exp p) i m = (i, m, Pid_Exp p) 
-- toProcMap (Diff_Exp e1 e2) i m =
--   let (i1, m1, e1') = toProcMap e1 i m
--       (i2, m2, e2') = toProcMap e2 i1 m1
--   in (i2, m2, Diff_Exp e1' e2')
toProcMap (If_Exp e1 e2 e3) i m =
  let (i1, m1, e1') = toProcMap e1 i m
      (i2, m2, e2') = toProcMap e2 i1 m1
      (i3, m3, e3') = toProcMap e3 i2 m2
  in (i3, m3, If_Exp e1' e2' e3')
toProcMap (Var_Exp id) i m = (i, m, Var_Exp id)
toProcMap (Let_Exp id e1 e2) i m =
  let (i1, m1, e1') = toProcMap e1 i m
      (i2, m2, e2') = toProcMap e2 i1 m1
  in (i2, m2, Let_Exp id e1' e2')
toProcMap (Letrec_Exp binds e) i m =
  let (i1, m1, binds') = 
        foldl (\(i', m', acc) (id, aname, arg, e1) ->
          let (i2, m2, e1') = toProcMap e1 i' m'
          in (i2, m2, (id, aname, arg, e1') : acc)) (i, m, []) binds
      (i2, m2, e') = toProcMap e i1 m1
  in (i2, m2, Letrec_Exp (reverse binds') e')
toProcMap (Proc_Exp aname id e) i m =
  let (i1, m1, e') = toProcMap e i m
      i2 = i1 + 1
      m2 = Map.insert i1 (Proc_Exp aname id e') m1
  in (i2, m2, PtrTo_Exp i1) -- Most important change! 
toProcMap (Call_Exp e1 e2) i m =
  let (i1, m1, e1') = toProcMap e1 i m
      (i2, m2, e2') = toProcMap e2 i1 m1
  in (i2, m2, Call_Exp e1' e2')
toProcMap (Block_Exp exps) i m =
  let (i1, m1, exps') = 
        foldl (\(i', m', acc) e ->
          let (i2, m2, e') = toProcMap e i' m'
          in (i2, m2, e':acc)) (i, m, []) exps
  in (i1, m1, Block_Exp (reverse exps'))
toProcMap (Set_Exp id e) i m =
  let (i1, m1, e') = toProcMap e i m
  in (i1, m1, Set_Exp id e')
toProcMap (Const_List_Exp nums) i m = (i, m, Const_List_Exp nums)
toProcMap (Unary_Exp op e) i m =
  let (i1, m1, e') = toProcMap e i m
  in (i1, m1, Unary_Exp op e')
toProcMap (Binary_Exp op e1 e2) i m =
  let (i1, m1, e1') = toProcMap e1 i m
      (i2, m2, e2') = toProcMap e2 i1 m1
  in (i2, m2, Binary_Exp op e1' e2')
toProcMap (Comp_Exp op e1 e2) i m =
  let (i1, m1, e1') = toProcMap e1 i m
      (i2, m2, e2') = toProcMap e2 i1 m1
  in (i2, m2, Comp_Exp op e1' e2')
toProcMap (Send_Exp exps) i m =
  let (i1, m1, exps') = 
        foldl (\(i', m', acc) e ->
          let (i2, m2, e') = toProcMap e i' m'
          in (i2, m2, e':acc)) (i, m, []) exps
  in (i1, m1, Send_Exp (reverse exps'))
toProcMap (Ready_Exp e) i m =
  let (i1, m1, e') = toProcMap e i m
  in (i1, m1, Ready_Exp e')
toProcMap (New_Exp e) i m =
  let (i1, m1, e') = toProcMap e i m
  in (i1, m1, New_Exp e')
-- toProcMap (Spawn_Exp e) i m =
--   let (i1, m1, e') = toProcMap e i m
--   in (i1, m1, Spawn_Exp e')
toProcMap (Tuple_Exp exps) i m =
  let (i1, m1, exps') =
        foldl (\(i', m', acc) e ->
                let (i2, m2, e') = toProcMap e i' m'
                in (i2, m2, acc ++ [e'])) (i, m, []) exps
  in (i1, m1, Tuple_Exp exps')
toProcMap (LetTuple_Exp ids e1 e2) i m =
  let (i1, m1, e1') = toProcMap e1 i m
      (i2, m2, e2') = toProcMap e2 i1 m1
  in (i2, m2, LetTuple_Exp ids e1' e2')
toProcMap (Append_Exp id e) i m =
  let (i1, m1, e') = toProcMap e i m
  in (i1, m1, Append_Exp id e') 
toProcMap (PowMod_Exp e1 e2 e3) i m =
  let (i1, m1, e1') = toProcMap e1 i m
      (i2, m2, e2') = toProcMap e2 i1 m1
      (i3, m3, e3') = toProcMap e3 i2 m2
  in (i3, m3, PowMod_Exp e1' e2' e3')
toProcMap (PtrTo_Exp n) i m = (i, m, PtrTo_Exp n) -- Should not happen!   
  