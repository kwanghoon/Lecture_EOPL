{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TyEnv where

import Expr(Identifier, Type(..))
import Data.List(lookup)

data TyEnv = 
    Empty_tyenv
  | Extend_tyenv Identifier Type TyEnv
  deriving Show

empty_tyenv :: TyEnv
empty_tyenv = Empty_tyenv

extend_tyenv :: Identifier -> Type -> TyEnv -> TyEnv
extend_tyenv var ty tyenv = Extend_tyenv var ty tyenv 

extend_tyenv_with :: [Identifier] -> [Type] -> TyEnv -> TyEnv
extend_tyenv_with [] [] tyenv = tyenv
extend_tyenv_with (var:vars) (ty:tys) tyenv = 
  extend_tyenv var ty (extend_tyenv_with vars tys tyenv)

apply_tyenv :: TyEnv -> Identifier -> Either String Type 
apply_tyenv Empty_tyenv var = Left $ "Variable not found: " ++ var
apply_tyenv (Extend_tyenv v ty tyenv) var
  | var == v = Right ty
  | otherwise = apply_tyenv tyenv var
