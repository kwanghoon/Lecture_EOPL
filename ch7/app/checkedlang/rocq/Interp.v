Set Warnings "-masking-absolute-name".
Unset Guard Checking.

From Stdlib Require Import String ZArith Bool.
Require Import Expr Env.

Module Interp.

Module Import ExprNS := Expr.
Module Import EnvNS := Env.

Open Scope string_scope.
Open Scope Z_scope.

Inductive EvalResult : Type :=
| EvalValue (val : ExpVal)
| EvalRuntimeError
| EvalGasExhausted.

Definition bind_eval (r : EvalResult) (k : ExpVal -> EvalResult) : EvalResult :=
  match r with
  | EvalValue v => k v
  | EvalRuntimeError => EvalRuntimeError
  | EvalGasExhausted => EvalGasExhausted
  end.

Fixpoint value_of (step : nat) (exp : Exp) (env : Env) {struct step} : EvalResult :=
  match step with
  | O => EvalGasExhausted
  | S step' =>
      match exp with
      | Const_Exp n => EvalValue (Num_Val n)
      | Var_Exp var =>
          match apply_env env var with
          | Some v => EvalValue v
          | None => EvalRuntimeError
          end
      | Diff_Exp e1 e2 =>
          bind_eval (value_of step' e1 env) (fun v1 =>
          bind_eval (value_of step' e2 env) (fun v2 =>
            match v1, v2 with
            | Num_Val n1, Num_Val n2 => EvalValue (Num_Val (n1 - n2))
            | _, _ => EvalRuntimeError
            end))
      | IsZero_Exp e =>
          bind_eval (value_of step' e env) (fun v =>
            match v with
            | Num_Val n => EvalValue (Bool_Val (Z.eqb n 0))
            | _ => EvalRuntimeError
            end)
      | If_Exp cond_exp then_exp else_exp =>
          bind_eval (value_of step' cond_exp env) (fun v =>
            match v with
            | Bool_Val true => value_of step' then_exp env
            | Bool_Val false => value_of step' else_exp env
            | _ => EvalRuntimeError
            end)
      | Let_Exp var exp1 body =>
          bind_eval (value_of step' exp1 env) (fun val1 =>
            value_of step' body (extend_env var val1 env))
      | Letrec_Exp _ proc_name bound_var _ proc_body letrec_body =>
          value_of step' letrec_body (extend_env_rec proc_name bound_var proc_body env)
      | Proc_Exp var _ body =>
          EvalValue (Proc_Val (procedure var body env))
      | Call_Exp rator rand =>
          bind_eval (value_of step' rator env) (fun proc_val =>
          bind_eval (value_of step' rand env) (fun arg_val =>
            match proc_val with
            | Proc_Val p => apply_procedure step' p arg_val
            | _ => EvalRuntimeError
            end))
      end
  end
with apply_procedure (step : nat) (proc : Proc) (arg : ExpVal) {struct step} : EvalResult :=
  match step with
  | O => EvalGasExhausted
  | S step' =>
      match proc with
      | Procedure var body saved_env =>
          value_of step' body (extend_env var arg saved_env)
      end
  end.

Definition initEnv : Env := empty_env.

Definition value_of_program (step : nat) (exp : Exp) : EvalResult :=
  value_of step exp initEnv.

End Interp.

Export Interp.

Set Warnings "+masking-absolute-name".
