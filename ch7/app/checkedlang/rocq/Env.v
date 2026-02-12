Set Warnings "-masking-absolute-name".

From Stdlib Require Import String ZArith Bool.
Require Import Expr.

Module Env.

Module Import ExprNS := Expr.

Open Scope string_scope.
Open Scope Z_scope.

Inductive Env : Type :=
| Empty_env
| Extend_env (saved_var : Identifier) (saved_val : ExpVal) (saved_env : Env)
| Extend_env_rec (proc_name bound_var : Identifier) (proc_body : Exp) (saved_env : Env)
with ExpVal : Type :=
| Num_Val (num : Z)
| Bool_Val (flag : bool)
| Proc_Val (proc : Proc)
with Proc : Type :=
| Procedure (var : Identifier) (body : Exp) (saved_env : Env).

Definition DenVal := ExpVal.

Definition empty_env : Env := Empty_env.

Definition extend_env (x : Identifier) (v : ExpVal) (env : Env) : Env :=
  Extend_env x v env.

Definition extend_env_rec (f x : Identifier) (body : Exp) (env : Env) : Env :=
  Extend_env_rec f x body env.

Definition procedure (var : Identifier) (body : Exp) (env : Env) : Proc :=
  Procedure var body env.

Definition expval_num (v : ExpVal) : option Z :=
  match v with
  | Num_Val n => Some n
  | _ => None
  end.

Definition expval_bool (v : ExpVal) : option bool :=
  match v with
  | Bool_Val b => Some b
  | _ => None
  end.

Definition expval_proc (v : ExpVal) : option Proc :=
  match v with
  | Proc_Val p => Some p
  | _ => None
  end.

Definition proc_var (p : Proc) : Identifier :=
  match p with
  | Procedure var _ _ => var
  end.

Definition proc_body (p : Proc) : Exp :=
  match p with
  | Procedure _ body _ => body
  end.

Definition proc_saved_env (p : Proc) : Env :=
  match p with
  | Procedure _ _ saved_env => saved_env
  end.

Fixpoint apply_env (env : Env) (search_var : Identifier) : option ExpVal :=
  match env with
  | Empty_env => None
  | Extend_env saved_var saved_val saved_env =>
      if String.eqb search_var saved_var
      then Some saved_val
      else apply_env saved_env search_var
  | Extend_env_rec proc_name bound_var proc_body saved_env =>
      if String.eqb search_var proc_name
      then Some (Proc_Val (Procedure bound_var proc_body env))
      else apply_env saved_env search_var
  end.

End Env.

Export Env.

Set Warnings "+masking-absolute-name".
