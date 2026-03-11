{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TypeCheck where

import Expr
import TyEnv

--
typeCheck :: Program -> IO (Either String Type)
typeCheck program = return (type_of_program program )

--
type_of_program :: Program -> Either String Type
type_of_program exp =
  do type_of exp initTyEnv

initTyEnv = extend_tyenv "x" TyInt empty_tyenv

--
type_of :: Exp -> TyEnv -> Either String Type

type_of (Const_Exp n) tyenv = Right TyInt

type_of (Var_Exp var) tyenv = apply_tyenv tyenv var

type_of (Diff_Exp exp1 exp2) tyenv =
  do ty1 <- type_of exp1 tyenv
     ty2 <- type_of exp2 tyenv
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> Right TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

type_of (Sum_Exp exp1 exp2) tyenv =
  do ty1 <- type_of exp1 tyenv
     ty2 <- type_of exp2 tyenv
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> Right TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

type_of (IsZero_Exp exp1) tyenv =
  do ty1 <- type_of exp1 tyenv
     case ty1 of
       TyInt -> Right TyBool
       _     -> expectedButErr TyInt ty1 exp1

type_of exp@(If_Exp exp1 exp2 exp3) tyenv =
  do condTy <- type_of exp1 tyenv
     thenTy <- type_of exp2 tyenv
     elseTy <- type_of exp3 tyenv
     case condTy of
       TyBool -> if equalType thenTy elseTy
                 then Right thenTy
                 else inequalIfBranchTyErr thenTy elseTy exp2 exp

       _      -> expectedButErr TyBool condTy exp1

type_of (Let_Exp letBindings body) tyenv =
  do tys <- types_of_exps (map snd letBindings) tyenv 
     type_of body 
       (extend_tyenv_with (map fst letBindings) tys tyenv)

type_of (Letrec_Exp letrecBindings letrec_body) tyenv =
  let vars = map (\(_, f, _, _) -> f) letrecBindings 
      tys  = map (\(resTy, _, tyVarList, _) -> 
                     TyFun (map fst tyVarList) resTy) letrecBindings 
      tyenv1 = extend_tyenv_with vars tys tyenv  
  in do mapM_ (\(resTy, f, tyVarList, proc_body)-> 
                  do let argTys = map fst tyVarList 
                     let argVars = map snd tyVarList
                     let tyenv2 = extend_tyenv_with argVars argTys tyenv1
                     procbodyTy <- type_of proc_body tyenv2
                     if equalType resTy procbodyTy
                     then Right ()
                     else expectedButErr resTy procbodyTy proc_body) letrecBindings 
        type_of letrec_body tyenv1

type_of (Proc_Exp tyVarList body) tyenv =
  do let argTys = map fst tyVarList
         vars   = map snd tyVarList
     bodyTy <- type_of body (extend_tyenv_with vars argTys tyenv)
     Right (TyFun argTys bodyTy)

type_of exp@(Call_Exp rator randList) tyenv =
  do ratorTy  <- type_of rator tyenv
     randTys <- types_of_exps randList tyenv
     type_of_call ratorTy randTys randList exp

type_of (Block_Exp []) tyenv = 
  Left $ "Empty block is not allowed"

type_of (Block_Exp (exp:expList)) tyenv =
  let type_of_begins exp expList =
       do ty <- type_of exp tyenv 
          if null expList then Right ty
          else type_of_begins (head expList) (tail expList)
  in  type_of_begins exp expList

type_of exp@(Set_Exp var rhsExp) tyenv =
  do lhsTy <- apply_tyenv tyenv var 
     rhsTy <- type_of rhsExp tyenv 
     if equalType rhsTy lhsTy then Right TyVoid
     else expectedButErr lhsTy rhsTy exp

type_of (List_Exp []) tyenv =
  Left $ "Cannot type check the empty list"

type_of (List_Exp (exp:expList)) tyenv =
  do ty <- type_of exp tyenv 
     let type_of_list expList =
          if null expList then Right (TyListOf ty)
          else do let exp = head expList
                  ty1 <- type_of exp tyenv 
                  if equalType ty ty1 then type_of_list (tail expList)
                  else expectedButErr ty ty1 exp
     type_of_list expList 

types_of_exps :: [Exp] -> TyEnv -> Either String [Type]
types_of_exps [] tyenv = Right []
types_of_exps (exp:exps) tyenv = 
  do ty <- type_of exp tyenv 
     tys <- types_of_exps exps tyenv 
     Right (ty:tys)

type_of_call :: Type -> [Type] -> [Exp] -> Exp -> Either String Type
type_of_call (TyFun argTyList _resTy) randTyList argList exp
  | length argTyList == length randTyList =
      do type_of_args randTyList argTyList argList
         Right _resTy
      
  | otherwise = wrongNumberOfArgsErr argTyList randTyList exp
type_of_call funTy _ _ exp = expectedFuntyButErr funTy exp

type_of_args :: [Type] -> [Type] -> [Exp] -> Either String ()
type_of_args [] [] [] = Right ()
type_of_args (randTy:randTys) (argTy:argTys) (rand:rands) = 
  if equalType randTy argTy then
    type_of_args randTys argTys rands 
  else 
    expectedButErr randTy argTy rand
type_of_args randTys argTys exps = 
  wrongNumberOfArgsErr3 randTys argTys exps
    

-- Utilities
expectedButErr expectedTy gotTy exp =
  Left $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr gotTy exp =
  Left $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

inequalIfBranchTyErr thenTy elseTy exp2 exp3 =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show thenTy ++ " in " ++ show exp2
          ++ "\t" ++ show elseTy ++ " in " ++ show exp3

inequalArgtyErr argTy1 argTy2 funexp argexp =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show argTy1 ++ " for the arugment of " ++ show funexp
          ++ "\t" ++ show argTy2 ++ " in " ++ show argexp

wrongNumberOfArgsErr _argTyList argTyList exp =
  Left $ "Wrong number of arguments: \n"
          ++ "\t" ++ show _argTyList ++ "\n"
          ++ "\t" ++ show argTyList ++ " in " ++ show exp      


wrongNumberOfArgsErr3 _argTyList argTyList exps =
  Left $ "Wrong number of arguments: \n"
          ++ "\t" ++ show _argTyList ++ "\n"
          ++ "\t" ++ show argTyList ++ "\n" 
          ++ "\t" ++ show exps             

equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType TyVoid TyVoid = True
equalType (TyFun tyList1 ty1') (TyFun tyList2 ty2') =
  equalTypes tyList1 tyList2 && equalType ty1' ty2'
equalType (TyListOf ty1) (TyListOf ty2) = equalType ty1 ty2
equalType _ _ = False

equalTypes :: [Type] -> [Type] -> Bool
equalTypes tyList1 tyList2 =
  and $ zipWith equalType tyList1 tyList2