Set Warnings "-masking-absolute-name".

From Stdlib Require Import String Bool.
Require Import Expr.

Module TyEnv.

Module Import ExprNS := Expr.

Open Scope string_scope.
Open Scope type_scope.

Inductive TyEnv : Type :=
| Empty_tyenv
| Extend_tyenv (var : Identifier) (ty : Ty) (saved_env : TyEnv).

Definition empty_tyenv : TyEnv := Empty_tyenv.

Definition extend_tyenv (var : Identifier) (ty : Ty) (env : TyEnv) : TyEnv :=
  Extend_tyenv var ty env.

Definition TyResult := (string + Ty)%type.

Fixpoint apply_tyenv (env : TyEnv) (var : Identifier) : TyResult :=
  match env with
  | Empty_tyenv => inl ("Variable not found: " ++ var)
  | Extend_tyenv saved_var saved_ty saved_env =>
      if String.eqb var saved_var
      then inr saved_ty
      else apply_tyenv saved_env var
  end.

End TyEnv.

Export TyEnv.

Set Warnings "+masking-absolute-name".
