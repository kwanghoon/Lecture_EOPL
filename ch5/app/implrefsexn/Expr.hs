module Expr(Program,Exp(..),UnaryOp(..),Identifier,PET(..),fromExp,fromExpList) where

type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp
  | Letrec_Exp Identifier Identifier Exp Exp -- letrec f1(x1) = expr1 in expr
  | Proc_Exp   Identifier Exp                      -- proc
  | Call_Exp   Exp Exp                             -- call
  | Block_Exp  [Exp]
  | Set_Exp    Identifier Exp
  | Const_List_Exp   [Int]                   -- number list
  | Unary_Exp  UnaryOp Exp                   -- null?, car, cdr
  | Try_Exp    Exp Identifier Exp            -- try exp catch exn exp
  | Raise_Exp  Exp                           -- raise exp  
  deriving Show

data UnaryOp = IsZero | IsNull | Car | Cdr deriving Show

type Identifier = String

--- Parsed Expression Tree

data PET =
    PET_ExpList {expListFrom :: [Exp] }
  | PET_Exp {expFrom :: Exp}
  deriving Show

fromExp exp                 = PET_Exp exp
fromExpList expList         = PET_ExpList expList