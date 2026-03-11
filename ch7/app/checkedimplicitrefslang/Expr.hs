{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expr(Program(..),Exp(..),Identifier,
            PET(..), Type(..),
            fromExp,fromType,
            fromExpList,fromIdExpList,fromTypeIdTypeIdListExpList,fromIdList,fromTypeList,fromTypeIdList,
            LetRecBindings) where

-- Untyped class-based expression language

type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | Sum_Exp    Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    LetBindings Exp    -- let bindings in expr
  | Letrec_Exp LetRecBindings Exp -- letrec rec_bindings in expr
  | Proc_Exp   [ (Type, Identifier) ] Exp   -- proc
  | Call_Exp   Exp [Exp]          -- e1 e2 ... en (n >= 1)
  | Block_Exp  [Exp]
  | Set_Exp    Identifier Exp
  | List_Exp   [Exp]              -- list(e1, ..., en)
  deriving Show  

type Identifier = String

-- x1 = expr1  ...  xn = exprn
type LetBindings = [ (Identifier, Exp) ]  

-- f1(x11,...,xn1) = expr1  ...  fk(xk1,...,xkn) = exprk
type LetRecBindings = [(Type, Identifier, [ (Type, Identifier) ], Exp)] 

data Type =
    TyInt
  | TyBool
  | TyVoid
  | TyFun [Type] Type
  | TyListOf Type -- listof(type)
  deriving (Eq, Show)


--- Parsed Expression Tree

data PET =
    PET_TypeIdTypeIdListExpList {typeIdTypeIdListExpListFrom :: [(Type, Identifier, [(Type, Identifier)], Exp)] }
  | PET_IdExpList {idExpListFrom :: [(Identifier, Exp)] }
  | PET_ExpList {expListFrom :: [Exp] }
  | PET_Exp {expFrom :: Exp}
  | PET_Type {typeFrom :: Type}
  | PET_IdList {idListFrom :: [Identifier] }
  | PET_TypeList {tyListFrom :: [Type] }
  | PET_TypeIdList {typeIdListFrom :: [(Type, Identifier)] }
  deriving Show

fromExp exp                 = PET_Exp exp
fromType ty                 = PET_Type ty
fromExpList expList         = PET_ExpList expList
fromIdExpList idExpList     = PET_IdExpList idExpList

fromTypeIdTypeIdListExpList typeIdTypeIdListExpList 
                            = PET_TypeIdTypeIdListExpList typeIdTypeIdListExpList
fromIdList idList           = PET_IdList idList
fromTypeList tyList         = PET_TypeList tyList
fromTypeIdList typeIdList   = PET_TypeIdList typeIdList


-- for testing the type checker
-- type TestCaseName = String
-- type ExprText = String

-- data TypeDeclTestCase = TDTC TestCaseName ExprText (Maybe Type)

-- data TypeDeclTestSuite = TypeDeclTestSuite [ TypeDeclTestCase ]