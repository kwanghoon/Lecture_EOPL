module TypeCheckerTest where

import Expr(Type(..))
import Testcase

typechecker_tests :: TestSuite
typechecker_tests =
  TestSuite
   [ 
      -- simple applications
      TYCK "apply_proc_in_rator_pos.checked" (Just TyInt),
      RUN  "apply_proc_in_rator_pos.checked" (Just "29"),
      TDTC "checker-doesnt-ignore-type-info-in-proc"
         "(proc(x : (int -> int)) -(x,1)  30)"
         Nothing,
       
      TYCK "apply_simple_proc.checked" (Just TyInt),
      RUN  "apply_simple_proc.checked" (Just "29"),  
      TYCK "let_to_proc_1.checked" (Just TyInt),
      RUN  "let_to_proc_1.checked" (Just "29"),
 
      TYCK "nested_procs_1.checked" (Just TyInt),
      RUN  "nested_procs_1.checked" (Just "-1"),
      TYCK "nested_procs_2.checked" (Just TyInt),
      RUN  "nested_procs_2.checked" (Just "2"),
       
      -- simple letrecs
      TYCK "simple_letrec_1.checked" (Just TyInt),
      RUN  "simple_letrec_1.checked" (Just "32"),
      TYCK "simple_letrec_2.checked" (Just TyInt),
      RUN  "simple_letrec_2.checked" (Just "8"),
 
      TYCK "simple_letrec_3.checked" (Just TyInt),
      RUN  "simple_letrec_3.checked" (Just "20"),
 
      TDTC "double-it" "letrec int double (n : int) = if zero?(n) then 0 \
                                 \ else -( (double -(n,1)), -2) \
                                 \ in (double 3)"
         (Just TyInt),
 
      -- tests of expressions that produce procedures
      TDTC "build-a-proc-typed" "proc (x : int) -(x,1)" (Just (TyFun TyInt TyInt)),
 
      TDTC "build-a-proc-typed-2" "proc (x : int) zero?(-(x,1))" (Just (TyFun TyInt TyBool)),
       
      TDTC "bind-a-proc-typed"
         "let f = proc (x : int) -(x,1) in (f 4)"
         (Just TyInt),
 
      TDTC "bind-a-proc-return-proc"
         "let f = proc (x : int) -(x,1) in f"
         (Just (TyFun TyInt TyInt)),
 
      TDTC "type-a-ho-proc-1"
         "proc(f : (int -> bool)) (f 3)"
         (Just (TyFun (TyFun TyInt TyBool) TyBool)),
 
      TDTC "type-a-ho-proc-2"
         "proc(f : (bool -> bool)) (f 3)"
         Nothing,
 
      TDTC "apply-a-ho-proc"
         "proc (x : int) proc (f : (int -> bool)) (f x)"
         (Just (TyFun TyInt (TyFun (TyFun TyInt TyBool) TyBool))),
 
      TDTC "apply-a-ho-proc-2"
         "proc (x : int) proc (f : (int -> (int -> bool))) (f x)"
         (Just (TyFun TyInt (TyFun (TyFun TyInt (TyFun TyInt TyBool)) (TyFun TyInt TyBool)))),
 
      TDTC "apply-a-ho-proc-3"
         "proc (x : int) proc (f : (int -> (int -> bool))) (f zero?(x))"
         Nothing,
 
      TDTC "apply-curried-proc"
         "((proc(x : int) proc (y : int)-(x,y)  4) 3)"
         (Just TyInt),
 
      TDTC "apply-a-proc-2-typed"
         "(proc (x : int) -(x,1) 4)" 
         (Just TyInt),
 
      TDTC "apply-a-letrec" " \
              \ letrec int f(x : int) = -(x,1) \
              \ in (f 40)"
                      (Just TyInt),
 
      TDTC "letrec-non-shadowing"
                 "(proc (x : int) \
                    \ letrec bool loop(x : bool) =(loop x) \ 
                    \  in x \
                    \ 1)"
         (Just TyInt),
 
       
      TDTC "letrec-return-fact" " \
                \ let times = proc (x : int) proc (y : int) -(x,y) \   
                \ in letrec \
                \      int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1))) \
                \    in fact"
         (Just (TyFun TyInt TyInt)),
 
      TDTC "letrec-apply-fcn" " \
           \ let times = proc (x : int) proc (y : int) -(x,y) \
           \ in letrec \
           \     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1))) \
           \   in (fact 4)"
         (Just TyInt),
 
      TYCK "modules_declare_and_ignore.simpmod" (Just TyInt),
      RUN  "modules_declare_and_ignore.simpmod" (Just "33"),
 
      TYCK "modules_take_one_value.simpmod" (Just TyInt),
      RUN  "modules_take_one_value.simpmod" (Just "3"),  

      -- ?? : same with modules-take-one-value
      TYCK "modules_take_one_value_no_import.simpmod" (Just TyInt),
      RUN  "modules_take_one_value_no_import.simpmod" (Just "3"),  

      -- Parse error: These examples are for PROCMODULES only. 
      -- TYCK "modules_take_from_parameterized_module.simpmod" Nothing,
      -- RUN  "modules_take_from_parameterized_module.simpmod" Nothing,
      -- TDTC "modules-take-from-parameterized-module" "\
      --       \ module m \
      --       \     interface \
      --       \        ((m1 : []) => [u : int]) \
      --       \     body \
      --       \        module-proc (m1 : []) [u = 3] \
      --       \ from m take u"
      --    Nothing,  
      TYCK "modules_check_iface_subtyping_1.simpmod" (Just TyInt),
      RUN  "modules_check_iface_subtyping_1.simpmod" (Just "3"),  
      
      -- if the interpreter always called the typechecker, or put
      -- only declared variables in the module, this would raise an
      -- error.  Exercise: make this modification.
      TYCK "modules_take_one_value_but_interface_bad.simpmod" Nothing,
      RUN  "modules_take_one_value_but_interface_bad.simpmod" Nothing,
      
      TYCK "modules_take_bad_value.simpmod" Nothing,
      RUN  "modules_take_bad_value.simpmod" Nothing,  

      TYCK "modules_two_vals.simpmod" (Just TyInt),
      RUN  "modules_two_vals.simpmod" (Just "11"),  

      TYCK "modules_two_vals_bad_interface_1.simpmod" Nothing,
      RUN  "modules_two_vals_bad_interface_1.simpmod" Nothing,  

      TYCK "modules_extra_vals_are_ok_1.simpmod" (Just TyInt),
      RUN  "modules_extra_vals_are_ok_1.simpmod" (Just "3"),  

      TYCK "modules_extra_vals_are_ok_2.simpmod" (Just TyInt),
      RUN  "modules_extra_vals_are_ok_2.simpmod" (Just "4"),


      TYCK "modules_extra_types_are_ok_11.opaque" (Just TyInt),
      RUN  "modules_extra_types_are_ok_11.opaque" (Just "4"),

      TYCK "modules_extra_types_are_ok_12.opaque" (Just TyInt),
      RUN  "modules_extra_types_are_ok_12.opaque" (Just "4"),

      TYCK "modules_extra_types_are_ok_13.opaque" (Just TyInt),
      RUN  "modules_extra_types_are_ok_13.opaque" (Just "4"),
      
      TYCK "modules_two_vals_bad_interface_14.simpmod" Nothing,
      RUN  "modules_two_vals_bad_interface_14.simpmod" Nothing,

      TYCK "modules_check_letstar_1.simpmod" (Just TyInt),
      RUN  "modules_check_letstar_1.simpmod" (Just "11"),

      TYCK "modules_check_letstar_2_0.simpmod" (Just TyInt),
      RUN  "modules_check_letstar_2_0.simpmod" (Just "11"),

      TYCK "modules_check_letstar_2_05.simpmod" (Just TyInt),
      RUN  "modules_check_letstar_2_05.simpmod" (Just "33"),

      TYCK "modules_check_letstar_2_1.simpmod" (Just TyInt),
      RUN  "modules_check_letstar_2_1.simpmod" (Just "11"),

      TYCK "modules_check_letstar_2_2.simpmod" Nothing,
      RUN  "modules_check_letstar_2_2.simpmod" Nothing,

      TYCK "modules_check_parameterized_1.procmod" (Just TyInt),
      RUN  "modules_check_parameterized_1.procmod" (Just "32"),

      TYCK "modules_check_parameterized_bad_argument.procmod" Nothing,
      RUN  "modules_check_parameterized_bad_argument.procmod" Nothing,

      TYCK "modules_check_parameterized_bad_interface_1.procmod" Nothing,
      RUN  "modules_check_parameterized_bad_interface_1.procmod" Nothing,

      TYCK "modules_check_parameterized_2.procmod" Nothing,
      RUN  "modules_check_parameterized_2.procmod" Nothing,

      -- modules declaring types

      TYCK "modules_export_abs_type_1.opaque" (Just TyInt),
      RUN  "modules_export_abs_type_1.opaque" (Just "33"),

      TYCK "modules_take_from_ints_0_1.opaque" (Just TyInt),
      RUN  "modules_take_from_ints_0_1.opaque" (Just "33"),
      
      TYCK "modules_take_from_ints_0_1a.opaque" (Just (TyQualified "m1" "t")),
      RUN  "modules_take_from_ints_0_1a.opaque" (Just "0"),

      TYCK "modules_take_from_ints_0_1_91.opaque" (Just TyBool),
      RUN  "modules_take_from_ints_0_1_91.opaque" (Just "True"),

      TYCK "modules_take_from_ints_0_1_91a.opaque" (Just (TyFun (TyQualified "m1" "t") (TyBool))),
      RUN  "modules_take_from_ints_0_1_91a.opaque" (Just "\"<proc>\""),

      TYCK "modules_take_from_ints_0_2.opaque" (Just TyBool),
      RUN  "modules_take_from_ints_0_2.opaque" (Just "True"),

      TYCK "modules_mybool_1.opaque" (Just TyBool),
      RUN  "modules_mybool_1.opaque" (Just "False"),

      TYCK "modules_mybool_1a.opaque" (Just (TyFun (TyQualified "mybool" "t") (TyBool))),
      RUN  "modules_mybool_1a.opaque" (Just "\"<proc>\""),

      TYCK "modules_mybool_1b.opaque" (Just (TyQualified "mybool" "t")),
      RUN  "modules_mybool_1b.opaque" (Just "1"),

      TYCK "modules_take_from_ints_1.opaque" (Just TyBool),
      RUN  "modules_take_from_ints_1.opaque" (Just "True"),

      TYCK "modules_take_from_ints_1a.opaque" (Just (TyFun (TyQualified "ints1" "t") (TyQualified "ints1" "t"))),
      RUN  "modules_take_from_ints_1a.opaque" (Just "\"<proc>\""),

      TYCK "modules_take_from_ints_1b.opaque" (Just (TyFun (TyQualified "ints1" "t") (TyBool))),
      RUN  "modules_take_from_ints_1b.opaque" (Just "\"<proc>\""),

      TYCK "modules_take_from_ints_2.opaque" (Just TyInt),
      RUN  "modules_take_from_ints_2.opaque" (Just "33"),

      TYCK "modules_take_from_ints_2_bad_1.opaque" Nothing,
      RUN  "modules_take_from_ints_2_bad_1.opaque" Nothing,

      TYCK "modules_take_from_ints_3.opaque" Nothing,
      RUN  "modules_take_from_ints_3.opaque" Nothing,

      TYCK "modules_check_polymorphism_1.opaque" (Just (TyFun (TyQualified "m" "t") (TyQualified "m" "t"))),
      RUN  "modules_check_polymorphism_1.opaque" (Just "\"<proc>\""),

      TYCK "modules_check_polymorphism_1a.opaque" (Just (TyFun (TyQualified "m" "t") (TyQualified "m" "t"))),
      RUN  "modules_check_polymorphism_1a.opaque" (Just "\"<proc>\""),

      TYCK "modules_check_polymorphism_1b.opaque" (Just (TyFun (TyQualified "m" "t") (TyQualified "m" "t"))),
      RUN  "modules_check_polymorphism_1b.opaque" (Just "\"<proc>\""),

      TYCK "modules_check_shadowing_1.opaque" (Just TyBool),
      RUN  "modules_check_shadowing_1.opaque" (Just "False"),

      TYCK "modules_check_shadowing_1_8.opaque" (Just TyInt),
      RUN  "modules_check_shadowing_1_8.opaque" (Just "33"),

      TYCK "modules_check_shadowing_1_8a.opaque" (Just (TyQualified "ints1" "t")),
      RUN  "modules_check_shadowing_1_8a.opaque" (Just "0"),

      TYCK "modules_apply_param_module_0_1.procmod" (Just TyInt),
      RUN  "modules_apply_param_module_0_1.procmod" (Just "33"),

      TYCK "modules_apply_param_module_1.procmod" (Just TyBool),
      RUN  "modules_apply_param_module_1.procmod" (Just "True"),

      TYCK "transparent_0.opaque" (Just TyInt),
      RUN  "transparent_0.opaque" (Just "-1"),

      TYCK "transparent_1.opaque" Nothing,
      RUN  "transparent_1.opaque" Nothing,

      TYCK "transparent_2.opaque" (Just TyInt),
      RUN  "transparent_2.opaque" (Just "1"),

      TYCK "modules_add_double_1.procmod" (Just TyBool),
      RUN  "modules_add_double_1.procmod" (Just "False"),

      TYCK "diamond_1.procmod" (Just TyBool),
      RUN  "diamond_1.procmod" (Just "True"),

      TYCK "pass_around_ho_module_1.procmod" (Just TyInt),
      RUN  "pass_around_ho_module_1.procmod" (Just "33"),

      TYCK "modules_myints_0_1.opaque" (Just (TyQualified "ints1" "t")),
      RUN  "modules_myints_0_1.opaque" (Just "4"),

      TYCK "modules_myints_0_20.opaque" Nothing,
      RUN  "modules_myints_0_20.opaque" (Just "-4"),

      TYCK "modules_myints_0_2a.opaque" (Just (TyQualified "ints1" "t")),
      RUN  "modules_myints_0_2a.opaque" (Just "-4"),

      TYCK "modules_apply_param_module_1a.procmod" (Just (TyQualified "ints_2" "t")),
      RUN  "modules_apply_param_module_1a.procmod" (Just "-8"),

      TYCK "modules_apply_param_module_3.procmod" (Just TyInt),
      RUN  "modules_apply_param_module_3.procmod" (Just "2"),

      TYCK "modules_apply_param_module_4.procmod" (Just (TyQualified "int3" "t")),
      RUN  "modules_apply_param_module_4.procmod" (Just "-16"),

      TYCK "lift_type_from_scope_0_01.opaque" (Just TyInt),
      RUN  "lift_type_from_scope_0_01.opaque" (Just "33"),

      TYCK "lift_type_from_scope_0_1.opaque" (Just TyInt),
      RUN  "lift_type_from_scope_0_1.opaque" (Just "33"),

      TYCK "lift_type_from_scope_1.opaque" Nothing,
      RUN  "lift_type_from_scope_1.opaque" (Just "33"),

      TYCK "lift_type_from_scope_2.opaque" (Just (TyFun (TyQualified "m1" "t1") (TyQualified "m1" "t1"))),
      RUN  "lift_type_from_scope_2.opaque" (Just "\"<proc>\""),

      TYCK "lift_type_from_scope_3.opaque" Nothing,
      RUN  "lift_type_from_scope_3.opaque" Nothing,

      TYCK "modules_14_1.opaque" (Just TyInt),
      RUN  "modules_14_1.opaque" (Just "33"),

      TYCK "modules_14.opaque" (Just (TyFun (TyInt) (TyInt))),
      RUN  "modules_14.opaque" (Just "\"<proc>\""),

      TYCK "modules_14b.opaque" (Just TyInt),
      RUN  "modules_14b.opaque" (Just "3"),

      TYCK "modules_14b.opaque" (Just TyInt),
      RUN  "modules_14b.opaque" (Just "3"),

      TYCK "modules_14b.opaque" (Just TyInt),
      RUN  "modules_14b.opaque" (Just "3"),

      TYCK "modules_test_curry1.procmod" (Just TyInt),
      RUN  "modules_test_curry1.procmod" (Just "33"),

      TYCK "modules_test_curry2.procmod" (Just TyInt),
      RUN  "modules_test_curry2.procmod" (Just "33")

   ] 