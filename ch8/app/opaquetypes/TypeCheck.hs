{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TypeCheck where

-- import Debug.Trace 
import Expr
import TyEnv
import EitherState

--
typeCheck :: Program -> IO (Either String Type)
typeCheck program = 
    return $ _run (type_of_program program)

--
add_module_defns_to_tyenv :: [ ModuleDef ] -> TyEnv -> Either_ String TyEnv
add_module_defns_to_tyenv [] tyenv = _Right tyenv
add_module_defns_to_tyenv (ModuleDef m iface mbody : moddefs) tyenv = 
  do actual_iface <- interface_of mbody tyenv                 -- important!
     isSubIface <- 
        --trace ("\n[iface]: " ++ show iface ++ "\n[actual_iface]: " ++ show actual_iface ++ "\n")
        sub_iface actual_iface iface tyenv
     if isSubIface                                            -- important!
     then 
        do  iface' <- expand_iface m iface tyenv               -- important!
            let newtyenv = extend_tyenv_with_module m iface' tyenv       
            -- trace (show "\n[iface']: " ++ show iface' ++ "\n")
            add_module_defns_to_tyenv moddefs newtyenv 
     else _Left $ "In the module " ++ m
                   ++ "\n  expected interface: " ++ show iface
                   ++ "\n  actual interface: " ++ show actual_iface

interface_of :: ModuleBody -> TyEnv -> Either_ String Interface 
interface_of (DefnsModuleBody defs) tyenv = 
  do decls <- defns_to_decls defs tyenv
     _Right $ SimpleIface decls

defns_to_decls :: [ Definition ] -> TyEnv -> Either_ String [ Declaration ]
defns_to_decls [] tyenv = _Right []
defns_to_decls (ValDefn var exp : defs) tyenv = 
  do ty <- _onLeft (type_of exp tyenv) 
             (\errmsg -> _Left $ errmsg ++ " in the declaration of " ++ var)
     decls <- defns_to_decls defs (extend_tyenv var ty tyenv)
     _Right $ ValDecl var ty : decls

defns_to_decls (TypeDefn var ty : defs) tyenv =
  do expanded_ty <- expand_type ty tyenv  -- expand this type
     decls <- defns_to_decls defs (extend_tyenv_with_type var expanded_ty tyenv)
     _Right $ TransparentTypeDecl var ty : decls
                 

sub_iface :: Interface -> Interface -> TyEnv -> Either_ String Bool
sub_iface (SimpleIface decls1) (SimpleIface decls2) tyenv =
  sub_decls decls1 decls2 tyenv 

sub_decls :: [Declaration] -> [Declaration] -> TyEnv -> Either_ String Bool
sub_decls decls1 [] tyenv = _Right True 
sub_decls [] decls2 tyenv = _Right False
sub_decls (decl1:decls1) (decl2:decls2) tyenv =
  let name1 = name_of_decl decl1
      name2 = name_of_decl decl2
  in if name1 == name2
      then 
        do bHead <- sub_decl decl1 decl2 tyenv
           tyenv' <- extend_tyenv_with_decl decl1 tyenv
           bRest <- sub_decls decls1 decls2 tyenv'
           _Right $ bHead && bRest
      else 
        do tyenv' <- extend_tyenv_with_decl decl1 tyenv
           sub_decls decls1 (decl2:decls2) tyenv'

-- sub_decl is called only when x==y! by sub_decls
sub_decl :: Declaration -> Declaration -> TyEnv -> Either_ String Bool
sub_decl (ValDecl x ty1) (ValDecl y ty2) tyenv = equalExpandedType ty1 ty2 tyenv
sub_decl (TransparentTypeDecl x ty1) (TransparentTypeDecl y ty2) tyenv = 
  equalExpandedType ty1 ty2 tyenv
sub_decl (TransparentTypeDecl x ty1) (OpaqueTypeDecl y) tyenv = _Right True
sub_decl (OpaqueTypeDecl x) (OpaqueTypeDecl y) tyenv = _Right True
sub_decl _ _ _ = _Right False


extend_tyenv_with_decl :: Declaration -> TyEnv -> Either_ String TyEnv
extend_tyenv_with_decl (ValDecl var ty) tyenv = _Right tyenv 
extend_tyenv_with_decl (OpaqueTypeDecl var) tyenv =
  do newNum <- _fresh
     _Right $ extend_tyenv_with_type var (TyQualified ("$m" ++ show newNum) var) tyenv -- Fixed!!
extend_tyenv_with_decl (TransparentTypeDecl var ty) tyenv =
  do expanded_ty <- expand_type ty tyenv
     _Right $ extend_tyenv_with_type var expanded_ty tyenv

equalExpandedType :: Type -> Type -> TyEnv -> Either_ String Bool
equalExpandedType ty1 ty2 tyenv = 
  do expanded_ty1 <- expand_type ty1 tyenv
     expanded_ty2 <- expand_type ty2 tyenv
     _Right $ equalType expanded_ty1 expanded_ty2

--
type_of_program :: Program -> Either_ String Type
type_of_program (Program moddefs modbody) = 
  do tyenv <- add_module_defns_to_tyenv moddefs empty_tyenv
     type_of modbody tyenv
    
--
type_of :: Exp -> TyEnv -> Either_ String Type

type_of (Const_Exp n) tyenv = _Right TyInt

type_of (Var_Exp var) tyenv = apply_tyenv tyenv var

type_of (QualifiedVar_Exp m v) tyenv = lookup_qualified_var_in_tyenv m v tyenv

type_of (Diff_Exp exp1 exp2) tyenv =
  do ty1 <- type_of exp1 tyenv 
     ty2 <- type_of exp2 tyenv 
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> _Right TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

type_of (IsZero_Exp exp1) tyenv =
  do ty1 <- type_of exp1 tyenv
     case ty1 of
       TyInt -> _Right TyBool
       _     -> expectedButErr TyInt ty1 exp1

type_of exp@(If_Exp exp1 exp2 exp3) tyenv =
  do condTy <- type_of exp1 tyenv
     thenTy <- type_of exp2 tyenv
     elseTy <- type_of exp3 tyenv
     case condTy of
       TyBool -> if equalType thenTy elseTy
                 then _Right thenTy
                 else inequalIfBranchTyErr thenTy elseTy exp2 exp
                      
       _      -> expectedButErr TyBool condTy exp1

type_of (Let_Exp var exp1 body) tyenv =
  do expTy  <- type_of exp1 tyenv 
     bodyTy <- type_of body (extend_tyenv var expTy tyenv) 
     _Right bodyTy

type_of (Letrec_Exp ty proc_name bound_var bvar_ty proc_body letrec_body) tyenv =
  do expanded_funty <- expand_type (TyFun bvar_ty ty) tyenv
     expanded_bvar_ty <- expand_type bvar_ty tyenv
     expanded_ty <- expand_type ty tyenv
     let tyenv1 = extend_tyenv bound_var expanded_bvar_ty
                    (extend_tyenv proc_name expanded_funty tyenv)
     procbodyTy <- type_of proc_body tyenv1
     
     let tyenv2 = extend_tyenv proc_name expanded_funty tyenv
     letrecbodyTy <- type_of letrec_body tyenv2
     
     if equalType expanded_ty procbodyTy
       then _Right letrecbodyTy
       else expectedButErr expanded_ty procbodyTy proc_body

type_of (Proc_Exp var argTy body) tyenv =
  do expanded_argTy <- expand_type argTy tyenv
     bodyTy <- type_of body (extend_tyenv var expanded_argTy tyenv)
     _Right (TyFun expanded_argTy bodyTy)

type_of (Call_Exp rator rand) tyenv =
  do funTy <- type_of rator tyenv
     argTy <- type_of rand tyenv
     case funTy of
       TyFun ty1 ty2 -> if equalType ty1 argTy
                        then _Right ty2
                        else inequalArgtyErr ty1 argTy rator rand
       _             -> expectedFuntyButErr funTy rator
      
-- Type expansion
expand_type :: Type -> TyEnv -> Either_ String Type
expand_type TyInt tyenv = _Right TyInt
expand_type TyBool tyenv = _Right TyBool
expand_type (TyFun ty1 ty2) tyenv = 
  do ty1' <- expand_type ty1 tyenv
     ty2' <- expand_type ty2 tyenv
     _Right $ TyFun ty1' ty2'
expand_type (TyName n) tyenv = lookup_type_name_in_tyenv n tyenv
expand_type (TyQualified m t) tyenv = lookup_qualified_type_in_tyenv m t tyenv

-- Interface expansion
expand_iface :: Identifier -> Interface -> TyEnv -> Either_ String Interface
expand_iface m (SimpleIface decls) tyenv = 
  do decls' <- expand_decls m decls tyenv
     _Right $ SimpleIface decls'         

-- Declaration expansion
expand_decls :: Identifier -> [Declaration] -> TyEnv -> Either_ String [Declaration]
expand_decls m [] internal_tyenv = _Right []

expand_decls m (ValDecl x ty : decls) internal_tyenv = 
  do expanded_ty <- expand_type ty internal_tyenv
     decls' <- expand_decls m decls internal_tyenv
     _Right $ ValDecl x expanded_ty : decls'

expand_decls m (OpaqueTypeDecl x : decls) internal_tyenv =
  let expanded_type = TyQualified m x                -- important!
      new_internal_env = extend_tyenv_with_type x expanded_type internal_tyenv
  in do decls' <- expand_decls m decls new_internal_env
        _Right $ TransparentTypeDecl x expanded_type : decls'

expand_decls m (TransparentTypeDecl x ty : decls) internal_tyenv =
  do expanded_type <- expand_type ty internal_tyenv  -- important!
     let new_internal_env = extend_tyenv_with_type x expanded_type internal_tyenv
     decls' <- expand_decls m decls new_internal_env
     _Right $ TransparentTypeDecl x expanded_type : decls'


-- Utilities
expectedButErr expectedTy gotTy exp =
  _Left $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr gotTy exp =
  _Left $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

inequalIfBranchTyErr thenTy elseTy exp2 exp3 =
  _Left $ "Type mismatch: \n"
          ++ "\t" ++ show thenTy ++ " in " ++ show exp2
          ++ "\t" ++ show elseTy ++ " in " ++ show exp3

inequalArgtyErr argTy1 argTy2 funexp argexp =
  _Left $ "Type mismatch: \n"
          ++ "\t" ++ show argTy1 ++ " for the arugment of " ++ show funexp
          ++ "\t" ++ show argTy2 ++ " in " ++ show argexp

-- Type equality
equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType (TyFun ty1 ty1') (TyFun ty2 ty2') =
  equalType ty1 ty2 && equalType ty1' ty2'
equalType (TyQualified m1 t1) (TyQualified m2 t2) =
  m1 == m2 && t1 == t2
equalType (TyName id1) (TyName id2) = id1 == id2 -- Necessary?
equalType _ _ = False

