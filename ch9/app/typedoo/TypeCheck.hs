{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TypeCheck where

import Expr
import TyEnv
import EnvStore (DenVal(SelfObject_Val), append_field_names)
-- import Debug.Trace(trace)

--
typeCheck :: Program -> IO (Either String Type)
typeCheck program = return (type_of_program program )

--
type_of_program :: Program -> Either String Type
type_of_program (Program classDecls exp) =
  do clzEnv <- initializeStaticClassEnv classDecls
     check_class_decls clzEnv classDecls
     -- trace (show clzEnv) 
     type_of clzEnv exp initTyEnv

initTyEnv = extend_tyenv "x" TyInt empty_tyenv

--
type_of :: StaticClassEnv -> Exp -> TyEnv -> Either String Type

type_of clzEnv (Const_Exp n) tyenv = Right TyInt

type_of clzEnv (Var_Exp var) tyenv = apply_tyenv tyenv var

type_of clzEnv (Diff_Exp exp1 exp2) tyenv =
  do ty1 <- type_of clzEnv exp1 tyenv
     ty2 <- type_of clzEnv exp2 tyenv
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> Right TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

type_of clzEnv (Sum_Exp exp1 exp2) tyenv =
  do ty1 <- type_of clzEnv exp1 tyenv
     ty2 <- type_of clzEnv exp2 tyenv
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> Right TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

type_of clzEnv (IsZero_Exp exp1) tyenv =
  do ty1 <- type_of clzEnv exp1 tyenv
     case ty1 of
       TyInt -> Right TyBool
       _     -> expectedButErr TyInt ty1 exp1

type_of clzEnv exp@(If_Exp exp1 exp2 exp3) tyenv =
  do condTy <- type_of clzEnv exp1 tyenv
     thenTy <- type_of clzEnv exp2 tyenv
     elseTy <- type_of clzEnv exp3 tyenv
     case condTy of
       TyBool -> if equalType thenTy elseTy
                 then Right thenTy
                 else inequalIfBranchTyErr thenTy elseTy exp2 exp

       _      -> expectedButErr TyBool condTy exp1

type_of clzEnv (Let_Exp letBindings body) tyenv =
  do tys <- types_of_exps clzEnv (map snd letBindings) tyenv 
     type_of clzEnv body 
       (extend_tyenv_with (map fst letBindings) tys tyenv)

type_of clzEnv (Letrec_Exp letrecBindings letrec_body) tyenv =
  let vars = map (\(_, f, _, _) -> f) letrecBindings 
      tys  = map (\(resTy, _, tyVarList, _) -> 
                     TyFun (map fst tyVarList) resTy) letrecBindings 
      tyenv1 = extend_tyenv_with vars tys tyenv  
  in do mapM_ (\(resTy, f, tyVarList, proc_body)-> 
                  do let argTys = map fst tyVarList 
                     let argVars = map snd tyVarList
                     let tyenv2 = extend_tyenv_with argVars argTys tyenv1
                     procbodyTy <- type_of clzEnv proc_body tyenv2
                     if equalType resTy procbodyTy
                     then Right ()
                     else expectedButErr resTy procbodyTy proc_body) letrecBindings 
        type_of clzEnv letrec_body tyenv1

type_of clzEnv (Proc_Exp tyVarList body) tyenv =
  do let argTys = map fst tyVarList
         vars   = map snd tyVarList
     bodyTy <- type_of clzEnv body (extend_tyenv_with vars argTys tyenv)
     Right (TyFun argTys bodyTy)

type_of clzEnv exp@(Call_Exp rator randList) tyenv =
  do ratorTy  <- type_of clzEnv rator tyenv
     randTys <- types_of_exps clzEnv randList tyenv
     type_of_call clzEnv ratorTy randTys randList exp

type_of clzEnv (Block_Exp []) tyenv = 
  Left $ "Empty block is not allowed"

type_of clzEnv (Block_Exp (exp:expList)) tyenv =
  let type_of_begins exp expList =
       do ty <- type_of clzEnv exp tyenv 
          if null expList then Right ty
          else type_of_begins (head expList) (tail expList)
  in  type_of_begins exp expList

type_of clzEnv exp@(Set_Exp var rhsExp) tyenv =
  do lhsTy <- apply_tyenv tyenv var 
     rhsTy <- type_of clzEnv rhsExp tyenv 
     check_is_subtype clzEnv rhsTy lhsTy exp 
     Right TyVoid

type_of clzEnv (List_Exp []) tyenv =
  Left $ "Cannot type check the empty list"

type_of clzEnv (List_Exp (exp:expList)) tyenv =
  do ty <- type_of clzEnv exp tyenv 
     let type_of_list expList =
          if null expList then Right (TyListOf ty)
          else do let exp = head expList
                  ty1 <- type_of clzEnv exp tyenv 
                  if equalType ty ty1 then type_of_list (tail expList)
                  else expectedButErr ty ty1 exp
     type_of_list expList 

type_of clzEnv exp@(New_Object_Exp cname expList) tyenv =
  do argTys <- types_of_exps clzEnv expList tyenv
     case lookup_static_class clzEnv cname of 
        Nothing -> Left $ "Class " ++ cname ++ " is not found in " ++ show exp
        Just (AStaticClass _ _ _ _ mtyenv) -> 
          do mty <- find_method_type clzEnv cname initialize 
             type_of_call clzEnv mty argTys expList exp
             Right (TyClass cname)
        Just (AStaticInterface _) -> Left $ "Cannot instantiate an interface: " ++ cname

type_of clzEnv exp@(Method_Call_Exp exp1 mname expList) tyenv =
  do argTys <- types_of_exps clzEnv expList tyenv 
     objTy <- type_of clzEnv exp1 tyenv 
     case objTy of  -- Note: no check like this in the EOPL book
       TyClass clzName ->
         do mty <- find_method_type clzEnv clzName mname
            type_of_call clzEnv mty argTys expList exp
       _ -> expectedClasstyButErr objTy exp 

type_of clzEnv exp@(Super_Call_Exp mname expList) tyenv =
  do argTys <- types_of_exps clzEnv expList tyenv 
     objTy <- apply_tyenv tyenv self
     case objTy of  -- Note: no check like this in the EOPL book
       TyClass clzName ->
         do mty <- find_method_type clzEnv clzName mname
            type_of_call clzEnv mty argTys expList exp
       _ -> expectedClasstyButErr objTy exp 

type_of clzEnv Self_Exp tyenv = apply_tyenv tyenv self

type_of clzEnv (Cast_Exp exp cname) tyenv =
  do objTy <- type_of clzEnv exp tyenv
     case objTy of 
      TyClass _ -> Right (TyClass cname) -- objTy 
      _ -> expectedButErr (TyClass "...") objTy exp

type_of clzEnv (InstanceOf_Exp exp cname) tyenv =
  do objTy <- type_of clzEnv exp tyenv
     case objTy of 
      TyClass _ -> Right TyBool
      _ -> expectedButErr (TyClass "...") objTy exp

types_of_exps :: StaticClassEnv -> [Exp] -> TyEnv -> Either String [Type]
types_of_exps clzEnv [] tyenv = Right []
types_of_exps clzEnv (exp:exps) tyenv = 
  do ty <- type_of clzEnv exp tyenv 
     tys <- types_of_exps clzEnv exps tyenv 
     Right (ty:tys)

type_of_call :: StaticClassEnv -> Type -> [Type] -> [Exp] -> Exp -> Either String Type
type_of_call clzEnv (TyFun argTyList _resTy) randTyList argList exp
  | length argTyList == length randTyList =
      do type_of_args clzEnv randTyList argTyList argList
         Right _resTy
      
  | otherwise = wrongNumberOfArgsErr argTyList randTyList exp
type_of_call clzEnv funTy _ _ exp = expectedFuntyButErr funTy exp

type_of_args :: StaticClassEnv -> [Type] -> [Type] -> [Exp] -> Either String ()
type_of_args clzEnv [] [] [] = Right ()
type_of_args clzEnv (randTy:randTys) (argTy:argTys) (rand:rands) = 
  do check_is_subtype clzEnv randTy argTy rand
     type_of_args clzEnv randTys argTys rands 
type_of_args clzEnv randTys argTys exps = 
  wrongNumberOfArgsErr3 randTys argTys exps
    
check_is_subtype :: StaticClassEnv -> Type -> Type -> Exp -> Either String ()
check_is_subtype clzEnv randTy argTy exp = 
  if is_subtype clzEnv randTy argTy then Right () 
  else subtypeFailure randTy argTy exp

is_subtype :: StaticClassEnv -> Type -> Type -> Bool
is_subtype clzEnv (TyClass clzName1) (TyClass clzName2) = 
  statically_is_subclass clzEnv clzName1 clzName2
is_subtype clzEnv (TyFun argTys1 resTy1) (TyFun argTys2 resTy2) = 
  is_subtype_list clzEnv argTys1 argTys2 
    && is_subtype clzEnv resTy2 resTy1 
is_subtype clzEnv ty1 ty2 = equalType ty1 ty2

is_subtype_list :: StaticClassEnv -> [Type] -> [Type] -> Bool
is_subtype_list clzEnv [] [] = True
is_subtype_list clzEnv (ty1:tys1) (ty2:tys2) = 
  is_subtype clzEnv ty1 ty2 
    && is_subtype_list clzEnv tys1 tys2
is_subtype_list clzEnv _ _ = False    

statically_is_subclass :: StaticClassEnv -> Identifier -> Identifier -> Bool
statically_is_subclass clzEnv clzName1 clzName2 = 
  clzName1 == clzName2 ||
    case lookup_static_class clzEnv clzName1 of
      Nothing -> False
      Just (AStaticClass maybeSuperName1 ifaceNames1 fs ftys mtyenv) ->
            (case maybeSuperName1 of
               Just superName1 -> statically_is_subclass clzEnv superName1 clzName2 
               Nothing -> False) || clzName2 `elem` ifaceNames1
           
      Just (AStaticInterface absmdecls) -> False 
         -- Note: interfaces have no inheritance relationship in TYPED-OO.  

-- Static Class Environment
initializeStaticClassEnv :: [ClassDecl] -> Either String StaticClassEnv
initializeStaticClassEnv [] = Right []
initializeStaticClassEnv classDecls =
  init [("object", AStaticClass Nothing [] [] [] [])] classDecls 
  where
    init staticClzEnv [] = Right staticClzEnv
    init staticClzEnv (classDecl:classDecls) = 
      do newStaticClzEnv <- classDeclToStaticClass staticClzEnv classDecl 
         init newStaticClzEnv classDecls 

classDeclToStaticClass :: StaticClassEnv -> ClassDecl -> Either String StaticClassEnv -- (Identifier, StaticClass)
classDeclToStaticClass clzEnv (Class_Decl cname superName ifaceNames fieldTypeNames methodDecls) =
  case lookup_static_class clzEnv superName of 
    (Just (AStaticClass superCName superIfaceNames superFieldNames superFieldTypes superMethodTyEnv)) ->
        do check_no_dups allIfaceNames cname
           check_no_dups fNames cname
           check_no_dups (map idFromMethodDecl methodDecls) cname -- local method name
           Right (clzEnv ++ [(cname, AStaticClass (Just superName) allIfaceNames fNames fTypes methodTyEnv)])
            where
              allIfaceNames = superIfaceNames ++ ifaceNames
              fNames = superFieldNames ++ map snd fieldTypeNames
              fTypes = superFieldTypes ++ map fst fieldTypeNames
              methodTyEnv = merge_method_envs superMethodTyEnv (map methodDeclToTyEnv methodDecls)

    (Just (AStaticInterface superIfaceMethodTyEnv)) ->
        do check_no_dups allIfaceNames cname
           check_no_dups fNames cname
           check_no_dups (map idFromMethodDecl methodDecls) cname -- local method name
           Right (clzEnv ++ [(cname, AStaticClass (Just superName) allIfaceNames fNames fTypes methodTyEnv)])
            where
              allIfaceNames = ifaceNames
              fNames = map snd fieldTypeNames
              fTypes = map fst fieldTypeNames
              methodTyEnv = merge_method_envs superIfaceMethodTyEnv (map methodDeclToTyEnv methodDecls)

    Nothing ->
        Left ("No super class in " ++ cname) 
  
classDeclToStaticClass clzEnv (Interface_Decl ifaceName methodDecls) =
  do check_no_dups (map fst methodTyEnv) ifaceName
     Right (clzEnv ++ [(ifaceName, AStaticInterface methodTyEnv)])
    where
      methodTyEnv = map methodDeclToTyEnv methodDecls

methodDeclToTyEnv :: MethodDecl -> (Identifier, Type)
methodDeclToTyEnv (Method_Decl ty name tyArgs _) = (name, TyFun (map fst tyArgs) ty)
methodDeclToTyEnv (AbstractMethod_Decl ty name tyArgs) = (name, TyFun (map fst tyArgs) ty)

-- 
check_class_decls :: StaticClassEnv -> [ClassDecl] -> Either String ()
check_class_decls clzEnv [] = Right ()
check_class_decls clzEnv (clzDecl:clzDecls) =
  do check_class_decl clzEnv clzDecl 
     check_class_decls clzEnv clzDecls

check_class_decl :: StaticClassEnv -> ClassDecl -> Either String ()
check_class_decl clzEnv (Class_Decl cname superName ifaceNames fieldTypeNames methodDecls) =
  case lookup_static_class clzEnv cname of
    Nothing -> Left $ "Class " ++ cname ++ " is not found"
    Just (AStaticClass _superName _ifaceNames _fieldNames _fieldTypes mtyenv) -> 
      let -- allIfaceNames = _ifaceNames ++ ifaceNames 
          fieldNames = _fieldNames ++ map snd fieldTypeNames 
          fieldTypes = _fieldTypes ++ map fst fieldTypeNames
      in do -- check_no_dups allIfaceNames cname -- moved from classDeclToStaticClass
            -- check_no_dups fieldNames cname -- moved from classDeclToStaticClass
            check_method_decls clzEnv cname superName fieldNames fieldTypes methodDecls
            check_if_implements clzEnv cname ifaceNames -- not allIfaceNames
    Just (AStaticInterface mtyenv) -> Right ()
check_class_decl clzEnv (Interface_Decl ifaceName methodDecls) = 
  Right ()
  -- check_no_dups (map idFromMethodDecl methodDecls) ifaceName  -- moved from classDeclToStaticClass

check_method_decls :: StaticClassEnv -> Identifier -> Identifier -> [Identifier] -> [Type] -> [MethodDecl] -> Either String ()
check_method_decls clzEnv cname superName fieldNames fieldTypes [] = Right ()
check_method_decls clzEnv cname superName fieldNames fieldTypes (methodDecl:methodDecls) =
  do check_method_decl clzEnv cname superName fieldNames fieldTypes methodDecl
     check_method_decls clzEnv cname superName fieldNames fieldTypes methodDecls

check_method_decl :: StaticClassEnv -> Identifier -> Identifier -> [Identifier] -> [Type] -> MethodDecl -> Either String ()
check_method_decl clzEnv cname superName fieldNames fieldTypes (Method_Decl ty name tyArgs body) =
  let vars   = map snd tyArgs
      varTys = map fst tyArgs
      tyenv = extend_tyenv_with vars varTys 
                (extend_tyenv_with_self_and_super (TyClass cname) superName 
                  (extend_tyenv_with fieldNames fieldTypes empty_tyenv)) in
    do bodyTy <- type_of clzEnv body tyenv
       check_is_subtype clzEnv bodyTy ty body
       if name == initialize then Right ()
       else
         case lookup_static_class clzEnv superName of
           Nothing -> Right () 
           Just (AStaticClass _ _ _ _ mtyenv) -> 
             case lookup name mtyenv of 
               Just mty -> check_is_subtype clzEnv (TyFun varTys ty) mty body
               Nothing -> Right ()
           Just (AStaticInterface mtyenv) ->
             case lookup name mtyenv of 
               Just mty -> check_is_subtype clzEnv (TyFun varTys ty) mty body
               Nothing -> Right ()

check_method_decl clzEnv cname superName fieldNames fieldTypes (AbstractMethod_Decl ty name tyArgs) =
  Right () -- Deadcode: Abstract methods never appear in class definitions in TYPED-OO.

check_if_implements :: StaticClassEnv -> Identifier -> [Identifier] -> Either String ()
check_if_implements clzEnv cname [] = Right ()
check_if_implements clzEnv cname (iname:inames) =
  do check_if_implements_ clzEnv cname iname 
     check_if_implements clzEnv cname inames

check_if_implements_ :: StaticClassEnv -> Identifier -> Identifier -> Either String ()
check_if_implements_ clzEnv cname iname =
  case lookup_static_class clzEnv iname of
    Nothing -> Left $ "Interface " ++ iname ++ " is not found"
    Just (AStaticInterface iface_mtyenv) -> 
      case lookup_static_class clzEnv cname of
        Nothing -> Left $ "Class " ++ cname ++ " is not found"
        Just (AStaticClass _ _ _ _ clz_mtyenv) -> 
          check_mth_impl clzEnv iface_mtyenv clz_mtyenv
        Just (AStaticInterface _) -> 
          Left $ "Interface " ++ cname ++ " cannot implement another interface " ++ iname
    Just (AStaticClass _ _ _ _ _) -> 
      Left $ "class " ++ cname ++ " attempts to implement non-interface " ++ iname

check_mth_impl :: StaticClassEnv -> [(Identifier,Type)] -> [(Identifier,Type)] -> Either String ()
check_mth_impl clzEnv [] clz_mtyenv = Right ()
check_mth_impl clzEnv ((mname, iface_mty):iface_mtyenv) clz_mtyenv =
  case lookup mname clz_mtyenv of
    Just clz_mty -> 
      do check_is_subtype clzEnv clz_mty iface_mty (Var_Exp mname) -- Todo: The exp is ad-hoc!
         check_mth_impl clzEnv iface_mtyenv clz_mtyenv
    Nothing -> 
      Left $ "class attempts to implement missing method " ++ mname

check_no_dups :: [Identifier] -> Identifier -> Either String ()
check_no_dups [] clzName = Right ()
check_no_dups (x:xs) clzName = 
 if x `elem` xs then 
   Left $ "Duplicate names: " ++ x ++ " in the class or interface " ++ clzName
 else check_no_dups xs clzName 

-- Utilities
expectedButErr expectedTy gotTy exp =
  Left $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr gotTy exp =
  Left $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

expectedClasstyButErr gotTy exp =
  Left $ "Expected class type but got " ++ show gotTy ++ " in " ++ show exp

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

subtypeFailure randTy argTy exp =
  Left $ "Subtype failure: \n"
          ++ "\t" ++ show randTy ++ " is not a subtype of\n"
          ++ "\t" ++ show argTy ++ " in " ++ show exp                

equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType TyVoid TyVoid = True
equalType (TyClass clzId1) (TyClass clzId2) = clzId1 == clzId2
equalType (TyFun tyList1 ty1') (TyFun tyList2 ty2') =
  equalTypes tyList1 tyList2 && equalType ty1' ty2'
equalType (TyListOf ty1) (TyListOf ty2) = equalType ty1 ty2
equalType _ _ = False

equalTypes :: [Type] -> [Type] -> Bool
equalTypes tyList1 tyList2 =
  and $ zipWith equalType tyList1 tyList2