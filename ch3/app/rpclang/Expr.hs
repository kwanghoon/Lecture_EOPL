module Expr(Program,Exp(..),Identifier, fv) where

import qualified Data.Set as Set


type Program = Exp

data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp
  | Proc_Exp   Location Identifier Exp        -- proc
  | Call_Exp   Exp Location Exp               -- call
  deriving Show

type Identifier = String
type Location = String

fv :: Exp -> Set.Set Identifier
fv (Const_Exp _) = Set.empty
fv (Diff_Exp e1 e2) = fv e1 `Set.union` fv e2
fv (IsZero_Exp e) = fv e
fv (If_Exp e1 e2 e3) = fv e1 `Set.union` fv e2 `Set.union` fv e3
fv (Var_Exp x) = Set.singleton x
fv (Let_Exp x e1 e2) = Set.delete x (fv e1 `Set.union` fv e2)
fv (Proc_Exp loc x e) = Set.union (Set.delete x (fv e)) (Set.singleton loc)
fv (Call_Exp e1 _ e2) = fv e1 `Set.union` fv e2