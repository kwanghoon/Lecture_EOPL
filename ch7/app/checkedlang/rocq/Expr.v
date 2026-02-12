From Stdlib Require Import String ZArith.

Module Expr.

Open Scope string_scope.
Open Scope Z_scope.

Definition Identifier := string.

Inductive Ty : Type :=
| TyInt
| TyBool
| TyFun (dom cod : Ty).

Inductive Exp : Type :=
| Const_Exp (n : Z)
| Diff_Exp (e1 e2 : Exp)
| IsZero_Exp (e : Exp)
| If_Exp (cond then_branch else_branch : Exp)
| Var_Exp (name : Identifier)
| Let_Exp (name : Identifier) (exp1 body : Exp)
| Letrec_Exp (result_ty : Ty) (proc_name bound_var : Identifier)
             (bound_ty : Ty) (proc_body letrec_body : Exp)
| Proc_Exp (param : Identifier) (param_ty : Ty) (body : Exp)
| Call_Exp (rator rand : Exp).

Definition Program := Exp.

Inductive AST : Type :=
| ASTExp (exp : Exp)
| ASTTy (ty : Ty).

Definition toASTExp (exp : Exp) : AST := ASTExp exp.

Definition toASTTy (ty : Ty) : AST := ASTTy ty.

End Expr.

Export Expr.
