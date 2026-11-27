Set Warnings "-masking-absolute-name".

From Stdlib Require Import String Bool.
Require Import Expr TyEnv.

Module TypeCheck.

Module Import ExprNS := Expr.
Module Import TyEnvNS := TyEnv.

Open Scope string_scope.

Definition ret (ty : Ty) : TyResult := inr ty.

Definition bind (r : TyResult) (k : Ty -> TyResult) : TyResult :=
  match r with
  | inl err => inl err
  | inr v => k v
  end.

Fixpoint show_ty (ty : Ty) : string :=
  match ty with
  | TyInt => "TyInt"
  | TyBool => "TyBool"
  | TyFun ty1 ty2 => "(TyFun " ++ show_ty ty1 ++ " " ++ show_ty ty2 ++ ")"
  end.

Fixpoint show_exp (exp : Exp) : string :=
  match exp with
  | Const_Exp _ => "(Const_Exp)"
  | Diff_Exp e1 e2 => "(Diff_Exp " ++ show_exp e1 ++ " " ++ show_exp e2 ++ ")"
  | IsZero_Exp e => "(IsZero_Exp " ++ show_exp e ++ ")"
  | If_Exp e1 e2 e3 =>
      "(If_Exp " ++ show_exp e1 ++ " " ++ show_exp e2 ++ " " ++ show_exp e3 ++ ")"
  | Var_Exp v => "(Var_Exp " ++ v ++ ")"
  | Let_Exp v e1 body =>
      "(Let_Exp " ++ v ++ " " ++ show_exp e1 ++ " " ++ show_exp body ++ ")"
  | Letrec_Exp _ f x _ body rec_body =>
      "(Letrec_Exp " ++ f ++ " " ++ x ++ " " ++ show_exp body ++ " " ++ show_exp rec_body ++ ")"
  | Proc_Exp v _ body => "(Proc_Exp " ++ v ++ " " ++ show_exp body ++ ")"
  | Call_Exp rator rand => "(Call_Exp " ++ show_exp rator ++ " " ++ show_exp rand ++ ")"
  end.

Definition expectedButErr (expectedTy gotTy : Ty) (exp : Exp) : TyResult :=
  inl ("Expected " ++ show_ty expectedTy ++ " but got " ++ show_ty gotTy ++ " in " ++ show_exp exp).

Definition expectedFuntyButErr (gotTy : Ty) (exp : Exp) : TyResult :=
  inl ("Expected function type but got " ++ show_ty gotTy ++ " in " ++ show_exp exp).

Definition inequalIfBranchTyErr (thenTy elseTy : Ty) (thenExp elseExp : Exp) : TyResult :=
  inl ("Type mismatch: "
        ++ show_ty thenTy ++ " in " ++ show_exp thenExp
        ++ " vs " ++ show_ty elseTy ++ " in " ++ show_exp elseExp).

Definition inequalArgtyErr (expectedTy gotTy : Ty) (funExp argExp : Exp) : TyResult :=
  inl ("Type mismatch: "
        ++ show_ty expectedTy ++ " for the argument of " ++ show_exp funExp
        ++ " vs " ++ show_ty gotTy ++ " in " ++ show_exp argExp).

Fixpoint equal_ty (ty1 ty2 : Ty) : bool :=
  match ty1, ty2 with
  | TyInt, TyInt => true
  | TyBool, TyBool => true
  | TyFun a1 b1, TyFun a2 b2 => andb (equal_ty a1 a2) (equal_ty b1 b2)
  | _, _ => false
  end.

Fixpoint type_of (exp : Exp) (env : TyEnv) : TyResult :=
  match exp with
  | Const_Exp _ => ret TyInt
  | Var_Exp var => apply_tyenv env var
  | Diff_Exp e1 e2 =>
      bind (type_of e1 env) (fun ty1 =>
      bind (type_of e2 env) (fun ty2 =>
        match ty1 with
        | TyInt => match ty2 with
                   | TyInt => ret TyInt
                   | _ => expectedButErr TyInt ty2 e2
                   end
        | _ => expectedButErr TyInt ty1 e1
        end))
  | IsZero_Exp e1 =>
      bind (type_of e1 env) (fun ty1 =>
        match ty1 with
        | TyInt => ret TyBool
        | _ => expectedButErr TyInt ty1 e1
        end)
  | If_Exp e1 e2 e3 =>
      bind (type_of e1 env) (fun condTy =>
        match condTy with
        | TyBool =>
            bind (type_of e2 env) (fun thenTy =>
            bind (type_of e3 env) (fun elseTy =>
              if equal_ty thenTy elseTy
              then ret thenTy
              else inequalIfBranchTyErr thenTy elseTy e2 e3))
        | _ => expectedButErr TyBool condTy e1
        end)
  | Let_Exp var exp1 body =>
      bind (type_of exp1 env) (fun expTy =>
        type_of body (extend_tyenv var expTy env))
  | Letrec_Exp result_ty proc_name bound_var bound_ty proc_body letrec_body =>
      let env1 := extend_tyenv bound_var bound_ty
                    (extend_tyenv proc_name (TyFun bound_ty result_ty) env) in
      bind (type_of proc_body env1) (fun procbodyTy =>
        if equal_ty result_ty procbodyTy then
          let env2 := extend_tyenv proc_name (TyFun bound_ty result_ty) env in
          type_of letrec_body env2
        else expectedButErr result_ty procbodyTy proc_body)
  | Proc_Exp var argTy body =>
      bind (type_of body (extend_tyenv var argTy env)) (fun bodyTy =>
        ret (TyFun argTy bodyTy))
  | Call_Exp rator rand =>
      bind (type_of rator env) (fun funTy =>
      bind (type_of rand env) (fun argTy =>
        match funTy with
        | TyFun ty1 ty2 =>
            if equal_ty ty1 argTy then ret ty2
            else inequalArgtyErr ty1 argTy rator rand
        | _ => expectedFuntyButErr funTy rator
        end))
  end.

Definition type_of_program (exp : Exp) : TyResult :=
  let initTyEnv := empty_tyenv in
  type_of exp initTyEnv.

Definition typeCheck (exp : Exp) : TyResult :=
  type_of_program exp.

End TypeCheck.

Export TypeCheck.

Set Warnings "+masking-absolute-name".
