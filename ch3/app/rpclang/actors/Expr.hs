{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Actors.Expr(Program,Exp(..),Identifier,UnaryOp(..), pprint) where

type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier                 -- variable : x
  | Let_Exp    Identifier Exp Exp         -- let x = expression in expression
  | Letrec_Exp
      [(Identifier,Identifier,Exp)] Exp   -- letrec { ..., f_i(x_i) = expression_i, ... } in expression
  | Proc_Exp   Identifier Exp             -- proc ( identifier ) expression
  | Call_Exp   Exp Exp                    -- ( expression expression)
  | Block_Exp  [ Exp ]                    -- begin exp1; ...; expk end
  | Set_Exp    Identifier Exp             -- set x = expression
  | Spawn_Exp  Exp                        -- spawn ( expression )
  | Yield_Exp                             -- yield ()
  | Mutex_Exp                             -- mutex ()
  | Wait_Exp  Exp                         -- wait ( expression )
  | Signal_Exp  Exp                       -- signal ( expression )
  | Const_List_Exp   [Int]                -- number list : [ number1, ..., numberk ]
  | Unary_Exp  UnaryOp Exp                -- unop ( expression ) where unop is one of car, cdr, null?, zero? print
  -- | Try_Exp    Exp Identifier Exp         -- try exp catch exn exp
  -- | Raise_Exp  Exp                        -- raise exp

  -- For Actors
  | Send_Exp [ Exp ]                      -- send ( to , msgs ) -> send ( SendExpressionList )
  | Ready_Exp Exp                         -- ready ( expression ) 
  | New_Exp   Exp                         -- new ( expression )
  | Eq_Actor_Exp Exp Exp                  -- actor? ( actor, actor )

  -- For Tuple
  | Tuple_Exp [ Exp ]                     -- ( expression1, ..., expressionk )
  | LetTuple_Exp [ Identifier ] Exp Exp   -- let x1, ..., xk = expression in expression
  
  deriving Show

data UnaryOp = IsZero | IsNull | Car | Cdr | Print deriving Show

type Identifier = String

pprint :: Exp -> String
pprint (Const_Exp n) = show n
pprint (Diff_Exp e1 e2) = "-" ++ "(" ++ pprint e1 ++ " , " ++ pprint e2 ++ ")"
pprint (If_Exp e1 e2 e3) = "if " ++ pprint e1 ++ " then " ++ pprint e2 ++ " else " ++ pprint e3
pprint (Var_Exp id) = id
pprint (Let_Exp id e1 e2) = "let " ++ id ++ " = " ++ pprint e1 ++ " in " ++ pprint e2
pprint (Letrec_Exp lst e) = "letrec " ++ concatMap (\(id1, id2, e) -> id1 ++ "(" ++ id2 ++ ") = " ++ pprint e ++ "; ") lst ++ "in " ++ pprint e
pprint (Proc_Exp id e) = "proc " ++ "(" ++ id ++ ") " ++ pprint e
pprint (Call_Exp e1 e2) = "(" ++ pprint e1 ++ " " ++ pprint e2 ++ ")"
pprint (Block_Exp lst) = "begin " ++ concatMap (\e -> pprint e ++ "; ") lst ++ "end"
pprint (Set_Exp id e) = "set " ++ id ++ " = " ++ pprint e
pprint (Const_List_Exp lst) = "[" ++ concatMap (\n -> show n ++ ", ") lst ++ "]"
pprint (Unary_Exp op e) = show op ++ "(" ++ pprint e ++ ")"

pprint (Send_Exp lst) = "send (" ++ concatMap (\e -> pprint e ++ ", ") (init lst) ++ pprint (last lst) ++ ")"
pprint (Ready_Exp e) = "ready (" ++ pprint e ++ ")"
pprint (New_Exp e) = "new (" ++ pprint e ++ ")"
pprint (Eq_Actor_Exp e1 e2) = "actor? (" ++ pprint e1 ++ ", " ++ pprint e2 ++ ")"

pprint (Tuple_Exp lst) =  "(" ++ concatMap (\e -> pprint e ++ ", ") (init lst) ++ pprint (last lst) ++ ")"
pprint (LetTuple_Exp lst e1 e2) =  "let (" ++ concatMap (++ ", ") (init lst) ++ (last lst) ++ ") = " ++ pprint e1 ++ " in " ++ pprint e2
