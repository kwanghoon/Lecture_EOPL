module TypeCheckerTest where

import Expr(Type(..))
import Testcase

typechecker_tests :: TestSuite
typechecker_tests =
  TestSuite
   [
     -- [Test for Run]
     -- simple arithmetic
     RUN "positive_const.let" (Just "11"),
     RUN "negative_const.let" (Just "-33"),
     RUN "simple_arith_1.let" (Just "11"),

     -- nested arithmetic
     RUN "nested_arith_left.let" (Just "-11"),
     RUN "nested_arith_right.let" (Just "44"),

     -- simple variables
     RUN "test_var_1.let" (Just "10"),
     RUN "test_var_2.let" (Just "9"),
     RUN "test_var_3.let" (Just "-9"),

     -- simple unbound variables
     RUN "test_unbound_var_1.let" Nothing,
     RUN "test_unbound_var_2.let" Nothing, 

     -- simple conditionals
     RUN "if_true.let" (Just "3"),
     RUN "if_false.let" (Just "4"),

     -- test dynamic typechecking
     RUN "no_bool_to_diff_1.let" Nothing,
     RUN "no_bool_to_diff_2.let" Nothing,
     RUN "no_int_to_if.let" Nothing,

     -- make sure that the test and both arms get evaluated
     -- properly. 
     RUN "if_eval_test_true.let" (Just "3"),
     RUN "if_eval_test_false.let" (Just "4"),

     -- and make sure the other arm doesn't get evaluated.
     RUN "if_eval_test_true_2.let" (Just "3"),
     RUN "if_eval_test_false_2.let" (Just "4"),

     -- simple let
     RUN "simple_let_1.let" (Just "3"),

     -- make sure the body and rhs get evaluated
     RUN "eval_let_body.let" (Just "2"),
     RUN "eval_let_rhs.let" (Just "2"),

     -- check nested let and shadowing
     RUN "simple_nested_let.let" (Just "-1"),
     RUN "check_shadowing_in_body.let" (Just "4"),
     RUN "check_shadowing_in_rhs.let" (Just "2"),

     -- simple applications
     RUN "apply_proc_in_rator_pos.checked" (Just "29"),
     RUN "checker_doesnt_ignore_type_info_in_proc.checked" (Just "29"),
     RUN "apply_simple_proc.checked" (Just "29"),
     RUN "let_to_proc_1.checked" (Just "29"),

     RUN "nested_procs_1.checked" (Just "-1"),
     RUN "nested_procs_2.checked" (Just "-1"),

     RUN "y_combinator_1.checked" (Just "12"),

     -- simple letrecs
     RUN "simple_letrec_1.checked" (Just "32"),
     RUN "simple_letrec_2.checked" (Just "8"),
     RUN "simple_letrec_3.checked" (Just "20"),
     RUN "ho_nested_letrecs.checked" (Just "1"),


     -- [Tests for Check]
     TYCK "positive_const.let" (Just TyInt),
     TYCK "negative_const.let" (Just TyInt),
     TYCK "simple_arith_1.let" (Just TyInt),

     TYCK "nested_arith_left.let" (Just TyInt),
     TYCK "nested_arith_right.let" (Just TyInt),

     TYCK "test_var_1.let" (Just TyInt),
     TYCK "test_var_2.let" (Just TyInt),
     TYCK "test_var_3.let" (Just TyInt),

     TYCK "zero_test_1.let" (Just TyBool),
     TYCK "zero_test_2.let" Nothing,

     TYCK "test_unbound_var_1.let" Nothing,
     TYCK "test_unbound_var_2.let" Nothing, 

     TYCK "if_true.let" (Just TyInt),
     TYCK "if_false.let" (Just TyInt),

     TYCK "if_eval_test_true.let" (Just TyInt),
     TYCK "if_eval_test_false.let" (Just TyInt),
     TYCK "if_eval_then.let" (Just TyInt),
     TYCK "if_eval_else.let" (Just TyInt),

     TYCK "if_compare_arms.let" Nothing,
     TYCK "if_check_test_is_boolean.let" Nothing,

     TYCK "simple_let_1.let" (Just TyInt),

     TYCK "eval_let_body.let" (Just TyInt),
     TYCK "eval_let_rhs.let" (Just TyInt),

     TYCK "simple_nested_let.let" (Just TyInt),
     TYCK "check_shadowing_in_body.let" (Just TyInt),
     TYCK "check_shadowing_in_rhs.let" (Just TyInt),

     TYCK "apply_proc_in_rator_pos.checked" (Just TyInt),
     TYCK "checker_doesnt_ignore_type_info_in_proc.checked" Nothing,
     TYCK "apply_simple_proc.checked" (Just TyInt),
     TYCK "let_to_proc_1.checked" (Just TyInt),

     TYCK "nested_procs_1.checked" (Just TyInt),
     TYCK "nested_procs_2.checked" (Just TyInt),

     TYCK "simple_letrec_1.checked" (Just TyInt),
     TYCK "simple_letrec_2.checked" (Just TyInt),
     TYCK "simple_letrec_3.checked" (Just TyInt),

     TYCK "double_it.checked" (Just TyInt),

     -- tests of expressions that produce procedures
     TYCK "build_a_proc_typed_1.checked" (Just (TyFun TyInt TyInt)),
     TYCK "build_a_proc_typed_2.checked" (Just (TyFun TyInt TyBool)),
     TYCK "bind_a_proc_typed.checked" (Just TyInt),
     TYCK "bind_a_proc_return_proc.checked" (Just (TyFun TyInt TyInt)),

     TYCK "type_a_ho_proc_1.checked" (Just (TyFun (TyFun TyInt TyBool) TyBool)),
     TYCK "type_a_ho_proc_2.checked" Nothing,

     TYCK "apply_a_ho_proc_1.checked" (Just (TyFun TyInt (TyFun (TyFun TyInt TyBool) TyBool))),
     TYCK "apply_a_ho_proc_2.checked" (Just (TyFun TyInt (TyFun (TyFun TyInt (TyFun TyInt TyBool)) (TyFun TyInt TyBool)))),
     TYCK "apply_a_ho_proc_3.checked" Nothing,

     TYCK "apply_curried_proc.checked" (Just TyInt),
     TYCK "apply_a_proc_2_typed.checked" (Just TyInt),
     TYCK "apply_a_letrec.checked" (Just TyInt),

     TYCK "letrec_non_shadowing.checked" (Just TyInt),
     TYCK "letrec_return_fact.checked" (Just (TyFun TyInt TyInt)),
     TYCK "letrec_apply_fact.checked" (Just TyInt),

     TYCK "pair_typecheck_1.checkedwithpair" (Just (TyPair TyInt TyBool)),
     TYCK "pair_typecheck_2.checkedwithpair" (Just TyInt),
     TYCK "pair_typecheck_3.checkedwithpair" (Just TyBool),
     TYCK "pair_typecheck_4.checkedwithpair" (Just TyInt),
     TYCK "pair_typecheck_5.checkedwithpair" (Just (TyPair TyInt (TyPair TyInt TyBool))),
     TYCK "pair_typecheck_6.checkedwithpair" (Just TyBool)
   ]
