Set Warnings "-masking-absolute-name".

From Coq Require Import String.
Require Import Expr TyEnv Env Interp TypeCheck.

Module TypeSound.

Module Import ExprNS := Expr.
Module Import TyEnvNS := TyEnv.
Module Import EnvNS := Env.
Module Import InterpNS := Interp.
Module Import TypeCheckNS := TypeCheck.

(* Typing relation for environments and expressed values. *)
Inductive env_has_type : Env -> TyEnv -> Prop :=
| EnvEmptyHasType :
    env_has_type Empty_env Empty_tyenv
| EnvExtendHasType :
    forall env tenv var val ty,
      value_has_type val ty ->
      env_has_type env tenv ->
      env_has_type (Extend_env var val env) (Extend_tyenv var ty tenv)
| EnvExtendRecHasType :
    forall env tenv proc_name bound_var proc_body argTy resultTy,
      env_has_type env tenv ->
      type_of proc_body
        (extend_tyenv bound_var argTy
          (extend_tyenv proc_name (TyFun argTy resultTy) tenv)) = inr resultTy ->
      env_has_type (Extend_env_rec proc_name bound_var proc_body env)
                   (Extend_tyenv proc_name (TyFun argTy resultTy) tenv)
with value_has_type : ExpVal -> Ty -> Prop :=
| ValueHasTypeNum :
    forall n,
      value_has_type (Num_Val n) TyInt
| ValueHasTypeBool :
    forall b,
      value_has_type (Bool_Val b) TyBool
| ValueHasTypeProc :
    forall param body saved_env tenv argTy resultTy,
      env_has_type saved_env tenv ->
      type_of body (extend_tyenv param argTy tenv) = inr resultTy ->
      value_has_type (Proc_Val (Procedure param body saved_env))
                      (TyFun argTy resultTy).

Scheme env_has_type_mut := Induction for env_has_type Sort Prop
with value_has_type_mut := Induction for value_has_type Sort Prop.

Combined Scheme env_value_has_type_mut_ind from env_has_type_mut, value_has_type_mut.

Lemma equal_ty_refl : forall ty, equal_ty ty ty = true.
Proof.
  induction ty; simpl;
    try rewrite IHty1; try rewrite IHty2; reflexivity.
Qed.

Lemma equal_ty_eq : forall ty1 ty2, equal_ty ty1 ty2 = true -> ty1 = ty2.
Proof.
  induction ty1; destruct ty2; simpl; intros H;
    try discriminate.
  - reflexivity.
  - reflexivity.
  - apply andb_prop in H as [H1 H2].
    specialize (IHty1_1 ty2_1 H1).
    specialize (IHty1_2 ty2_2 H2).
    subst; reflexivity.
Qed.

Lemma env_has_type_extend_rec_preserves :
  forall env tenv proc_name bound_var proc_body argTy resultTy,
    env_has_type env tenv ->
    type_of proc_body
      (extend_tyenv bound_var argTy
        (extend_tyenv proc_name (TyFun argTy resultTy) tenv)) = inr resultTy ->
    env_has_type (extend_env_rec proc_name bound_var proc_body env)
                 (extend_tyenv proc_name (TyFun argTy resultTy) tenv).
Proof.
  intros env tenv proc_name bound_var proc_body argTy resultTy Henv Hty.
  now constructor.
Qed.

Lemma env_lookup_sound :
  forall env tenv, env_has_type env tenv ->
  forall search_var target_ty found_val,
    apply_tyenv tenv search_var = inr target_ty ->
    apply_env env search_var = Some found_val ->
    value_has_type found_val target_ty.
Proof.
  induction 1; intros search_var target_ty found_val Htyenv Happ;
    simpl in *.
  - discriminate.
  - destruct (String.eqb search_var var) eqn:Heq.
    + apply String.eqb_eq in Heq; subst.
      inversion Htyenv; inversion Happ; subst; assumption.
    + eapply IHenv_has_type; eassumption.
  - destruct (String.eqb search_var proc_name) eqn:Heq.
    + apply String.eqb_eq in Heq; subst.
      inversion Htyenv; subst.
      inversion Happ; subst.
      eapply ValueHasTypeProc with
        (tenv := extend_tyenv proc_name (TyFun argTy resultTy) tenv).
      * eapply EnvExtendRecHasType; eauto.
      * exact H0.
    + eapply IHenv_has_type; eassumption.
Qed.

Lemma env_extend_has_type :
  forall env tenv var val ty,
    env_has_type env tenv ->
    value_has_type val ty ->
    env_has_type (extend_env var val env) (extend_tyenv var ty tenv).
Proof.
  intros; now constructor.
Qed.

Lemma value_has_type_proc_intro :
  forall param body saved_env tenv argTy resultTy,
    env_has_type saved_env tenv ->
    type_of body (extend_tyenv param argTy tenv) = inr resultTy ->
    value_has_type (Proc_Val (procedure param body saved_env))
                   (TyFun argTy resultTy).
Proof.
  intros.
  eapply ValueHasTypeProc with (tenv := tenv); eauto.
Qed.

Lemma type_of_program_env :
  env_has_type initEnv
    (extend_tyenv "i" TyInt
      (extend_tyenv "v" TyInt
        (extend_tyenv "x" TyInt empty_tyenv))).
Proof.
  unfold initEnv.
  repeat (eapply EnvExtendHasType).
  - constructor.
  - constructor.
  - constructor.
  - constructor.
Qed.

Lemma bind_eval_value :
  forall r k v,
    bind_eval r k = EvalValue v ->
    exists x, r = EvalValue x /\ k x = EvalValue v.
Proof.
  intros [val| |] k v H;
    simpl in H;
    try discriminate.
  eauto.
Qed.

Lemma bind_inr :
  forall (r : TyResult) (k : Ty -> TyResult) ty,
    bind r k = inr ty ->
    exists v, r = inr v /\
              k v = inr ty.
Proof.
  intros [err|v] k ty H;
    simpl in H.
  - discriminate.
  - eauto.
Qed.

Lemma type_sound_mutual :
  forall step,
    (forall exp env tenv ty val,
        env_has_type env tenv ->
        type_of exp tenv = inr ty ->
        value_of step exp env = EvalValue val ->
        value_has_type val ty)
    /\
    (forall proc arg val argTy resultTy,
        value_has_type (Proc_Val proc) (TyFun argTy resultTy) ->
        value_has_type arg argTy ->
        apply_procedure step proc arg = EvalValue val ->
        value_has_type val resultTy).
Proof.
  induction step as [|step IH].
  - split.
    + intros exp env tenv ty val _ _ Heval.
      simpl in Heval.
      discriminate.
    + intros proc arg val argTy resultTy _ _ Happ.
      simpl in Happ.
      destruct proc; discriminate.
  - destruct IH as [IHenv IHproc].
    split.
    + intros exp env tenv ty val Henv Hty Heval.
      simpl in Heval.
      destruct exp as
          [n
          | e1 e2
          | e
          | cond_exp then_exp else_exp
          | var_name
          | var exp1 body
          | result_ty proc_name bound_var bound_ty proc_body letrec_body
          | param param_ty body_proc
          | rator rand]; simpl in *.
      * inversion Hty; inversion Heval; constructor.
      * apply bind_inr in Hty as [ty1 [Hty1 Hty]].
        apply bind_inr in Hty as [ty2 [Hty2 Hty]].
        destruct ty1; try (simpl in Hty; discriminate).
        destruct ty2; try (simpl in Hty; discriminate).
        inversion Hty; subst ty.
        simpl in Heval.
        apply bind_eval_value in Heval as [v1 [Heval1 Heval]].
        apply bind_eval_value in Heval as [v2 [Heval2 Heval]].
        pose proof (IHenv _ _ _ _ _ Henv Hty1 Heval1) as Hv1_ty.
        pose proof (IHenv _ _ _ _ _ Henv Hty2 Heval2) as Hv2_ty.
        inversion Hv1_ty; subst.
        inversion Hv2_ty; subst.
        inversion Heval; subst.
        constructor.
      * apply bind_inr in Hty as [ty1 [Hty1 Hty]].
        destruct ty1; try (simpl in Hty; discriminate).
        inversion Hty; subst ty.
        simpl in Heval.
        apply bind_eval_value in Heval as [v [Heval1 Heval]].
        pose proof (IHenv _ _ _ _ _ Henv Hty1 Heval1) as Hv_ty.
        inversion Hv_ty; subst.
        simpl in Heval.
        inversion Heval; subst.
        constructor.
      * apply bind_inr in Hty as [condTy [HcondTy Hty]].
        destruct condTy; try (simpl in Hty; discriminate).
        apply bind_inr in Hty as [thenTy [HthenTy Hty]].
        apply bind_inr in Hty as [elseTy [HelseTy Hty]].
        destruct (equal_ty thenTy elseTy) eqn:Heq; try (simpl in Hty; discriminate).
        simpl in Hty.
        inversion Hty; subst ty.
        apply equal_ty_eq in Heq; subst elseTy.
        simpl in Heval.
        apply bind_eval_value in Heval as [cond_val [Hcond Heval]].
        pose proof (IHenv _ _ _ _ _ Henv HcondTy Hcond) as Hcond_typed.
        inversion Hcond_typed; subst.
        destruct b;
          [ simpl in Heval;
            eapply IHenv with (exp := then_exp) (env := env) (tenv := tenv); eauto
          | simpl in Heval;
            eapply IHenv with (exp := else_exp) (env := env) (tenv := tenv); eauto ].
        * destruct (apply_env env var_name) eqn:Hlookup; inversion Heval; subst.
          eapply env_lookup_sound; eauto.
      * apply bind_inr in Hty as [expTy [HexpTy Hty]].
        simpl in Heval.
        apply bind_eval_value in Heval as [val1 [Hval1 Heval]].
        pose proof (IHenv _ _ _ _ _ Henv HexpTy Hval1) as Hval1_ty.
        pose proof (env_extend_has_type env tenv var val1 expTy Henv Hval1_ty) as Henv_ext.
        eapply IHenv with (exp := body)
                          (env := extend_env var val1 env)
                          (tenv := extend_tyenv var expTy tenv)
                          (ty := ty);
          eauto.
      * apply bind_inr in Hty as [procbodyTy [HprocTy Hty]].
        destruct (equal_ty result_ty procbodyTy) eqn:Heq; try (simpl in Hty; discriminate).
        simpl in Hty.
        apply equal_ty_eq in Heq; subst procbodyTy.
        pose proof (env_has_type_extend_rec_preserves _ _ _ _ _ _ _ Henv HprocTy) as Henv_rec.
        eapply IHenv with (exp := letrec_body)
                          (env := extend_env_rec proc_name bound_var proc_body env)
                          (tenv := extend_tyenv proc_name (TyFun bound_ty result_ty) tenv)
                          (ty := ty);
          eauto.
      * apply bind_inr in Hty as [bodyTy [HbodyTy Hty]].
        inversion Hty; subst ty.
        inversion Heval; subst.
        eapply value_has_type_proc_intro; eauto.
      * apply bind_inr in Hty as [funTy [HfunTy Hty]].
        apply bind_inr in Hty as [argTy [HargTy Hty]].
        destruct funTy as [| |dom cod]; try (simpl in Hty; discriminate).
        destruct (equal_ty dom argTy) eqn:Heq; try (simpl in Hty; discriminate).
        simpl in Hty.
        inversion Hty; subst ty.
        simpl in Heval.
        apply bind_eval_value in Heval as [proc_val [Hproc Heval]].
        apply bind_eval_value in Heval as [arg_val [Harg Heval]].
        pose proof (IHenv _ _ _ _ _ Henv HfunTy Hproc) as Hproc_ty.
        pose proof (IHenv _ _ _ _ _ Henv HargTy Harg) as Harg_ty.
        apply equal_ty_eq in Heq; subst argTy.
      destruct proc_val as [n|b|p]; simpl in Heval; try discriminate.
      eapply IHproc; eauto.
    + intros proc arg val argTy resultTy Hproc Harg Happ.
      destruct proc as [param body saved_env].
      simpl in Happ.
      inversion Hproc as [ | | param' body' saved_env' tenv' argTy' resultTy' Henv_saved HbodyTy]; subst.
      pose proof (env_extend_has_type saved_env tenv' param arg argTy Henv_saved Harg) as Henv_ext.
      eapply IHenv with (exp := body)
                        (env := extend_env param arg saved_env)
                        (tenv := extend_tyenv param argTy tenv')
                        (ty := resultTy);
        eauto.
Qed.

Lemma type_sound_env :
  forall step exp env tenv ty val,
    env_has_type env tenv ->
    type_of exp tenv = inr ty ->
    value_of step exp env = EvalValue val ->
    value_has_type val ty.
Proof.
  intros step exp env tenv ty val Henv Hty Heval.
  destruct (type_sound_mutual step) as [H _].
  eapply H; eauto.
Qed.

Lemma type_sound_proc :
  forall step proc arg val argTy resultTy,
    value_has_type (Proc_Val proc) (TyFun argTy resultTy) ->
    value_has_type arg argTy ->
    apply_procedure step proc arg = EvalValue val ->
    value_has_type val resultTy.
Proof.
  intros step proc arg val argTy resultTy Hproc Harg Happ.
  destruct (type_sound_mutual step) as [_ H].
  eapply H; eauto.
Qed.

Theorem type_sound :
  forall (step : nat) (exp : Exp) (ty : Ty) (val : ExpVal),
    type_of_program exp = inr ty ->
    value_of_program step exp = EvalValue val ->
    value_has_type val ty.
Proof.
  intros step exp ty val Htyped Heval.
  unfold type_of_program in Htyped.
  unfold value_of_program in Heval.
  eapply type_sound_env; eauto.
  apply type_of_program_env.
Qed.

End TypeSound.

Export TypeSound.

Set Warnings "+masking-absolute-name".
