{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module EnvStore where

import Ref(Location)
import Expr (Identifier,Exp,ClassDecl(..),MethodDecl(..))

import Data.Maybe
import Data.List (elemIndex,lookup)

-- Environment
data Env =
    Empty_env
  | Extend_env [Identifier] [Location] Env
  | Extend_env_rec [(Identifier,[Identifier],Exp)] Env
  | Extend_env_with_self_and_super Object Identifier Env
  deriving Show

apply_env :: Env -> Store -> Identifier -> (DenVal, Store)
apply_env Empty_env store search_var = error (search_var ++ " is not found.")
apply_env (Extend_env saved_vars saved_vals saved_env) store search_var = 
  if search_var `elem` saved_vars
    then (Loc_Val $ saved_vals !! (fromJust $ Data.List.elemIndex search_var saved_vars),store)
    else apply_env saved_env store search_var
apply_env (Extend_env_rec idIdExpList saved_env) store search_var
  | isIn      = let (l,store') = newref store procVal in (Loc_Val l,store')
  | otherwise = apply_env saved_env store search_var
  where isIn      = or [ p_name==search_var | (p_name,b_var,p_body) <- idIdExpList ]
        procVal = head [ Proc_Val (procedure b_var p_body (Extend_env_rec idIdExpList saved_env)) 
                       | (p_name,b_var,p_body) <- idIdExpList, p_name==search_var ]
apply_env (Extend_env_with_self_and_super obj super_name saved_env) store search_var
  | search_var == "%self"  = (SelfObject_Val obj,store)
  | search_var == "%super" = (SuperClassName_Val super_name, store)
  | otherwise = apply_env saved_env store search_var

-- Abstract data type interfaces for Env
empty_env :: Env
empty_env = Empty_env

extend_env :: [Identifier] -> [Location] -> Env -> Env
extend_env xs vs env = Extend_env xs vs env

extend_env_rec :: [(Identifier,[Identifier],Exp)] -> Env -> Env
extend_env_rec idIdListExpList env = Extend_env_rec idIdListExpList env

extend_env_with_self_and_super :: Object -> Identifier -> Env -> Env
extend_env_with_self_and_super obj super_name env = 
  Extend_env_with_self_and_super obj super_name env

-- Expressed values
data ExpVal =
    Num_Val {expval_num :: Int}
  | Bool_Val {expval_bool :: Bool}
  | Proc_Val {expval_proc :: Proc}
  -- | Ref_Val {expval_loc :: Location}
  | List_Val {expval_list :: [ExpVal]} -- Listof_Val?
  | Object_Val  {expval_obj :: Object}
  | Uninitialized_Val  -- for uninitialized fields (Not in the textbook)

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show proc  -- "<proc>"
  show (List_Val list) = "(" ++ showWithSp list ++ ")" -- Todo: print a list as [1,2,3] instead of (1,2,3)??
  show (Object_Val obj) = show obj
  show (Uninitialized_Val) = "<uninitialized>"

showWithSp :: [ExpVal] -> String
showWithSp [] = ""
showWithSp [x] = show x
showWithSp (x:xs) = show x ++ " " ++ showWithSp xs  

-- Denoted values
data DenVal = 
    Loc_Val {denval_loc :: Location} -- Ref(ExpVal) 
  | SelfObject_Val {denval_self :: Object} -- for %self
  | SuperClassName_Val {denval_super :: Identifier} -- for %super

-- Procedure values : data structures
data Proc = Procedure {proc_vars :: [Identifier], proc_body :: Exp, saved_env :: Env}

instance Show Proc where
  show (Procedure vars body saved_env) = show "<proc>"

procedure :: [Identifier] -> Exp -> Env -> Proc
procedure vars body env = Procedure vars body env

-- Object values : data structures
data Object = AnObject {object_class_name :: Identifier, object_fields :: [Location]}

instance Show Object where
  show (AnObject class_name fields) = 
    "<" ++ show class_name ++ ":object>"

-- In Interp.hs
-- apply_procedure :: Proc -> ExpVal -> ExpVal
  
-- | Store.hs
type Store = (Location, [(Location,ExpVal)]) -- Next new location

newref :: Store -> ExpVal -> (Location,Store)
newref (next,s) v = (next,(next+1,(next,v):s))

deref :: Store -> Location -> ExpVal
deref (next,s) loc =
  case [v | (loc',v) <- s, loc==loc'] of
    (v:_) -> v
    _     -> error ("Location not found: " ++ show loc)

setref :: Store -> Location -> ExpVal -> Store
setref (next,s) loc v = (next,update s)
  where update [] = error ("Invalid reference: " ++ show loc)
        update ((loc',w):s')
          | loc==loc' = (loc,v):s'
          | otherwise = (loc',w):update s'

initEnv :: Env
initEnv = extend_env ["i", "v", "x"] [1,2,3] empty_env

initStore :: Store
initStore = (4,[(1, Num_Val 1)
                , (2, Num_Val 5)
                , (3, Num_Val 10)])

-- Classes
data Class = AClass 
  { class_super_name :: Maybe Identifier, 
    class_field_names :: [Identifier], 
    class_method_env :: MethodEnv
  }
  deriving Show

new_object :: Identifier -> ClassEnv -> Store -> (Object, Store)
new_object class_name class_env store = 
  let field_names = class_field_names (lookup_class class_name class_env)
      (objFields, store') = 
        foldl mkUninitializedFields ([], store) field_names
        where
          mkUninitializedFields (fields,store) field_name = 
            let (loc,store') = newref store Uninitialized_Val in
              (fields++[loc],store')             
  in (AnObject class_name objFields, store')

-- Methods
data Method = AMethod 
  { method_vars :: [Identifier], 
    method_body :: Exp,
    method_super_name :: Identifier,
    method_field_names :: [Identifier]
  }
  deriving Show

-- apply_method in Interp.hs

-- Class Environments
type ClassEnv = [ (Identifier,Class) ]

initClassEnv :: ClassEnv
initClassEnv = [ ("object", AClass Nothing [] []) ]

add_to_class_env :: Identifier -> Class -> ClassEnv -> ClassEnv
add_to_class_env class_name aClass class_env = 
  (class_name,aClass) : class_env

lookup_class :: Identifier -> ClassEnv -> Class
lookup_class class_name class_env = 
  case Data.List.lookup class_name class_env of
    Just aClass -> aClass
    Nothing    -> error ("Class " ++ class_name ++ " not found.")

initialize_class_env :: [ClassDecl] -> ClassEnv
initialize_class_env classDecls = 
  foldl (flip initialize_class_decl) initClassEnv classDecls    

initialize_class_decl :: ClassDecl -> ClassEnv -> ClassEnv
initialize_class_decl (Class_Decl class_name super_name field_names method_decls) class_env = 
  let accumulated_field_names = 
        append_field_names
          (class_field_names (lookup_class super_name class_env))
          field_names
      aClass = AClass (Just super_name) accumulated_field_names
                  (merge_method_envs 
                    (class_method_env (lookup_class super_name class_env)) 
                      (method_decls_method_envs 
                        method_decls super_name accumulated_field_names))
  in add_to_class_env class_name aClass class_env

append_field_names :: [Identifier] -> [Identifier] -> [Identifier]
append_field_names super_fields new_fields =
  rename_super_fields 1 super_fields
  where
    rename_super_fields n [] = new_fields
    rename_super_fields n (f:fs)
      | f `elem` new_fields = (f ++ "_" ++ show n) : rename_super_fields (n+1) fs
      | otherwise           = f : rename_super_fields n fs

-- Method Environments
type MethodEnv = [ (Identifier, Method)]

find_method :: Identifier -> Identifier -> ClassEnv -> Method
find_method class_name method_name class_env = 
  let aClass = lookup_class class_name class_env 
      methodEnv = class_method_env aClass
  in case Data.List.lookup method_name methodEnv of
      Just method -> method
      Nothing     -> error ("Method " ++ method_name ++ " not found.")

method_decls_method_envs :: [MethodDecl] -> Identifier -> [Identifier] -> MethodEnv
method_decls_method_envs method_decls super_name field_names = 
  map method_decl_method_env method_decls
  where
    method_decl_method_env (Method_Decl method_name vars body) = 
      (method_name, AMethod vars body super_name field_names)

merge_method_envs :: MethodEnv -> MethodEnv -> MethodEnv
merge_method_envs superMethodEnvs newMethodEnvs = newMethodEnvs ++ superMethodEnvs
