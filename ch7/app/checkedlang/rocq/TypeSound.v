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

Lemma env_lookup_complete :
  forall env tenv, env_has_type env tenv ->
  forall search_var target_ty,
    apply_tyenv tenv search_var = inr target_ty ->
    exists found_val,
      apply_env env search_var = Some found_val /\
      value_has_type found_val target_ty.
Proof.
  induction 1; intros search_var target_ty Htyenv; simpl in *.
  - discriminate.
  - destruct (String.eqb search_var var) eqn:Heq.
    + exists val.
      split.
      * reflexivity.
      * inversion Htyenv; subst. assumption.
    + specialize (IHenv_has_type _ _ Htyenv) as [found_val [Happ Htyped]].
      exists found_val.
      split.
      * assumption.
      * assumption.
  - destruct (String.eqb search_var proc_name) eqn:Heq.
    + inversion Htyenv; subst.
      exists (Proc_Val (Procedure bound_var proc_body0
                  (Extend_env_rec proc_name bound_var proc_body0 env))).
      split.
        * reflexivity.
      * eapply ValueHasTypeProc with
          (tenv := extend_tyenv proc_name (TyFun argTy resultTy) tenv).
        -- eapply EnvExtendRecHasType; eauto.
        -- exact H0.
    + specialize (IHenv_has_type _ _ Htyenv) as [found_val [Happ Htyped]].
      exists found_val.
      split.
      * assumption.
      * assumption.
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

Lemma type_sound_value_or_gas_mutual :
  forall step,
    (forall exp env tenv ty,
        env_has_type env tenv ->
        type_of exp tenv = inr ty ->
        (exists v, value_of step exp env = EvalValue v /\ value_has_type v ty)
        \/ value_of step exp env = EvalGasExhausted)
    /\
    (forall proc arg argTy resultTy,
        value_has_type (Proc_Val proc) (TyFun argTy resultTy) ->
        value_has_type arg argTy ->
        (exists v, apply_procedure step proc arg = EvalValue v /\ value_has_type v resultTy)
        \/ apply_procedure step proc arg = EvalGasExhausted).
Proof.
  induction step as [|step IH].
  - split.
    + intros exp env tenv ty Henv Hty.
      right. simpl. reflexivity.
    + intros proc arg argTy resultTy Hproc Harg.
      right. simpl. reflexivity.
  - destruct IH as [IHexp IHproc].
    split.
    + intros exp env tenv ty Henv Hty.
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
      * inversion Hty; subst.
        left. exists (Num_Val n). split; [reflexivity|constructor].
      * apply bind_inr in Hty as [ty1 [Hty1 Hty]].
        apply bind_inr in Hty as [ty2 [Hty2 Hty]].
        destruct ty1; try (simpl in Hty; discriminate).
        destruct ty2; try (simpl in Hty; discriminate).
        inversion Hty; subst.
        specialize (IHexp e1 env tenv TyInt Henv Hty1) as IH1.
        specialize (IHexp e2 env tenv TyInt Henv Hty2) as IH2.
        destruct IH1 as [[v1 [He1 Hv1]]|Hg1].
        -- destruct IH2 as [[v2 [He2 Hv2]]|Hg2].
           ++ inversion Hv1; subst.
              inversion Hv2; subst.
              left. exists (Num_Val (BinInt.Z.sub n n0)).
              split.
              ** simpl. rewrite He1, He2. reflexivity.
              ** constructor.
           ++ right. simpl. rewrite He1, Hg2. reflexivity.
        -- right. simpl. rewrite Hg1. reflexivity.
      * apply bind_inr in Hty as [ty1 [Hty1 Hty]].
        destruct ty1; try (simpl in Hty; discriminate).
        inversion Hty; subst.
        specialize (IHexp e env tenv TyInt Henv Hty1) as IHe.
        destruct IHe as [[v [Hev Htv]]|Hg].
          -- destruct v as [nval|bval|pval].
             ++ left. exists (Bool_Val (BinInt.Z.eqb nval BinNums.Z0)).
              split.
              ** simpl. rewrite Hev. reflexivity.
              ** constructor.
            ++ inversion Htv.
            ++ inversion Htv.
        -- right. simpl. rewrite Hg. reflexivity.
      * apply bind_inr in Hty as [condTy [HcondTy Hty]].
        destruct condTy; try (simpl in Hty; discriminate).
        apply bind_inr in Hty as [thenTy [HthenTy Hty]].
        apply bind_inr in Hty as [elseTy [HelseTy Hty]].
        destruct (equal_ty thenTy elseTy) eqn:Heq; try (simpl in Hty; discriminate).
        inversion Hty; subst.
        apply equal_ty_eq in Heq; subst elseTy.
        specialize (IHexp cond_exp env tenv TyBool Henv HcondTy) as IHc.
        destruct IHc as [[cv [Hc Hcv]]|Hcg].
        -- inversion Hcv; subst.
           destruct b.
            ++ specialize (IHexp then_exp env tenv _ Henv HthenTy) as IHt.
              destruct IHt as [[v [Ht Hv]]|Htg].
              ** left. exists v. split; [simpl; rewrite Hc; exact Ht|exact Hv].
              ** right. simpl. rewrite Hc, Htg. reflexivity.
            ++ specialize (IHexp else_exp env tenv _ Henv HelseTy) as IHe.
              destruct IHe as [[v [He Hv]]|Heg].
              ** left. exists v. split; [simpl; rewrite Hc; exact He|exact Hv].
              ** right. simpl. rewrite Hc, Heg. reflexivity.
        -- right. simpl. rewrite Hcg. reflexivity.
      * specialize (env_lookup_complete _ _ Henv _ _ Hty) as [found_val [Hlookup Htyped]].
        left. exists found_val. split.
        -- simpl. rewrite Hlookup. reflexivity.
        -- exact Htyped.
      * apply bind_inr in Hty as [expTy [HexpTy Hty_body]].
        specialize (IHexp exp1 env tenv expTy Henv HexpTy) as IH1.
        destruct IH1 as [[v1 [He1 Hv1]]|Hg1].
        -- pose proof (env_extend_has_type env tenv var v1 expTy Henv Hv1) as Henv_ext.
           specialize (IHexp body (extend_env var v1 env) (extend_tyenv var expTy tenv) ty Henv_ext Hty_body)
             as IHbody.
           destruct IHbody as [[v [Hb Hvb]]|Hbg].
           ++ left. exists v. split; [simpl; rewrite He1; exact Hb|exact Hvb].
           ++ right. simpl. rewrite He1. exact Hbg.
        -- right. simpl. rewrite Hg1. reflexivity.
      * apply bind_inr in Hty as [procbodyTy [HprocTy Hty]].
        destruct (equal_ty result_ty procbodyTy) eqn:Heq; try (simpl in Hty; discriminate).
        apply equal_ty_eq in Heq; subst procbodyTy.
        pose proof (env_has_type_extend_rec_preserves _ _ _ _ _ _ _ Henv HprocTy) as Henv_rec.
        specialize (IHexp letrec_body
                    (extend_env_rec proc_name bound_var proc_body env)
                    (extend_tyenv proc_name (TyFun bound_ty result_ty) tenv)
                    ty Henv_rec Hty) as IHlr.
        exact IHlr.
      * apply bind_inr in Hty as [bodyTy [HbodyTy Hty]].
        inversion Hty; subst.
        left. exists (Proc_Val (Procedure param body_proc env)).
        split.
        -- reflexivity.
        -- eapply value_has_type_proc_intro; eauto.
      * apply bind_inr in Hty as [funTy [HfunTy Hty]].
        apply bind_inr in Hty as [argTy [HargTy Hty]].
        destruct funTy as [| |dom cod]; try (simpl in Hty; discriminate).
        destruct (equal_ty dom argTy) eqn:Heq; try (simpl in Hty; discriminate).
        inversion Hty; subst.
        apply equal_ty_eq in Heq; subst argTy.
          specialize (IHexp rator env tenv (TyFun dom ty) Henv HfunTy) as IHrator.
        specialize (IHexp rand env tenv dom Henv HargTy) as IHrand.
        destruct IHrator as [[proc_val [Hrator Hproc_typed]]|Hgr].
        -- destruct IHrand as [[arg_val [Hrand Harg_typed]]|Hga].
            ++ destruct proc_val as [n0|b0|procv].
              ** inversion Hproc_typed.
              ** inversion Hproc_typed.
              ** simpl in Hrator.
                specialize (IHproc procv arg_val dom ty Hproc_typed Harg_typed) as IHp.
                destruct IHp as [[v [Hp Hv]]|Hpg].
                --- left. exists v. split; [simpl; rewrite Hrator, Hrand; exact Hp|exact Hv].
                --- right. simpl. rewrite Hrator, Hrand. exact Hpg.
           ++ right. simpl. rewrite Hrator, Hga. reflexivity.
        -- right. simpl. rewrite Hgr. reflexivity.
    + intros proc arg argTy resultTy Hproc Harg.
      destruct proc as [param body saved_env].
      inversion Hproc as [| |param' body' saved_env' tenv' argTy' resultTy' Henv_saved HbodyTy]; subst.
      pose proof (env_extend_has_type saved_env tenv' param arg argTy Henv_saved Harg) as Henv_ext.
      specialize (IHexp body (extend_env param arg saved_env)
                  (extend_tyenv param argTy tenv') resultTy Henv_ext HbodyTy) as IHb.
      destruct IHb as [[v [Hb Hvb]]|Hbg].
      * left. exists v. split; [simpl; exact Hb|exact Hvb].
      * right. simpl. exact Hbg.
Qed.

Theorem type_sound_no_runtime_error :
  forall (step : nat) (exp : Exp) (ty : Ty),
    type_of_program exp = inr ty ->
    value_of_program step exp <> EvalRuntimeError.
Proof.
  intros step exp ty Htyped Hrt.
  destruct (type_sound_value_or_gas_mutual step) as [Hexp _].
  unfold type_of_program in Htyped.
  unfold value_of_program in Hrt.
  specialize (Hexp exp initEnv
              (extend_tyenv "i" TyInt
                (extend_tyenv "v" TyInt
                  (extend_tyenv "x" TyInt empty_tyenv)))
              ty type_of_program_env Htyped) as Hres.
  destruct Hres as [[v [Hev _]]|Hg].
  - rewrite Hev in Hrt. discriminate.
  - rewrite Hg in Hrt. discriminate.
Qed.

Theorem type_sound_value_or_gas_assuming_no_runtime :
  forall (step : nat) (exp : Exp) (ty : Ty),
    type_of_program exp = inr ty ->
    value_of_program step exp <> EvalRuntimeError ->
    (exists v, value_of_program step exp = EvalValue v /\ value_has_type v ty)
    \/ value_of_program step exp = EvalGasExhausted.
Proof.
  intros step exp ty Htyped Hno_runtime.
  destruct (value_of_program step exp) as [v| |] eqn:Heval.
  - left.
    exists v.
    split.
    + reflexivity.
    + eapply type_sound; [exact Htyped | exact Heval].
  - exfalso.
    apply Hno_runtime.
    reflexivity.
  - right.
    reflexivity.
Qed.

Theorem type_sound_value_or_gas :
  forall (step : nat) (exp : Exp) (ty : Ty),
    type_of_program exp = inr ty ->
    (exists v, value_of_program step exp = EvalValue v /\ value_has_type v ty)
    \/ value_of_program step exp = EvalGasExhausted.
Proof.
  intros step exp ty Htyped.
  eapply type_sound_value_or_gas_assuming_no_runtime; eauto.
  eapply type_sound_no_runtime_error; eauto.
Qed.

End TypeSound.

Export TypeSound.

Set Warnings "+masking-absolute-name".
