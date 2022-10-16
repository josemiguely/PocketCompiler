open Dev.Ast
open Dev.Parse
open Dev.Compile
open Dev.Interp
open Alcotest
open Bbctester.Test
open Printf

(* Testing arithmetic expression using the print function defined in Interp 
   and the default equality for comparison *)
let exp : tag expr testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_expr e)) (=)

let value : value testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_val e)) (=)


(* Tests for our [parse] function *)
let test_parse_int () =
  check exp "same int" (parse_exp (`Atom "5")) (Num (5L,-1))

let test_parse_var () =
  check exp "same var" (parse_exp (`Atom "x")) (Id ("x",-1))

let test_parse_bool () =
  check exp "same bool" (parse_exp (`Atom "true")) (Bool (true,-1))
  
let test_parse_tuple () =
    check exp "same tuple" (parse_exp (`List [`Atom "tup"; `Atom "true" ; `Atom "1"])) (Tuple ([(Bool(true,-1));(Num (1L,-1))],-1) )

let test_parse_empty_tuple () =
    check exp "empty tuple" (parse_exp (`List [`Atom "tup"])) (Tuple([],-1) )

let test_parse_add1 () =
  check exp "increment applies" 
  (parse_exp (`List [`Atom "add1" ; `Atom "1"])) 
  (Prim1 (Add1, Num (1L,-1),-1))

let test_parse_sub1 () =
  check exp "decrement applies" 
  (parse_exp (`List [`Atom "sub1" ; `Atom "1"])) 
  (Prim1 (Sub1, Num (1L,-1),-1))
  
let test_parse_add () =
  check exp "addition applies" 
  (parse_exp (`List [`Atom "+" ; `Atom "1" ; `Atom "7"])) 
  (Prim2 (Add, Num (1L,-1), Num (7L,-1),-1))

let test_parse_less () =
  check exp "lesser comparison applies" 
  (parse_exp (`List [`Atom "<=" ; `Atom "1" ; `Atom "7"])) 
  (Prim2 (Lte, Num (1L,-1), Num (7L,-1),-1))

let test_parse_and () =
  check exp "conjunction applies" 
  (parse_exp (`List [`Atom "and" ; `Atom "true" ; `Atom "false"])) 
  (Prim2 (And, Bool (true,-1), Bool (false,-1),-1))

let test_parse_get () =
    check exp "get applies" (parse_exp (`List [`Atom "get" ; `Atom "1";`Atom "2"])) (Prim2 (Get,Num (1L,-1), Num (2L,-1),-1))

let test_parse_set () =
    check exp "set applies" (parse_exp (`List [`Atom "set" ; `Atom "true";`Atom "37";`Atom "3"])) (Set (Bool(true,-1), Num (37L,-1),Num (3L,-1),-1))

let test_parse_fork () =
  check exp "if clause applies"
  (parse_exp (`List [`Atom "if" ; `Atom "true" ; `Atom "1" ; `Atom "0"])) 
  (If (Bool (true,-1), Num (1L,-1), Num (0L,-1),-1))

let test_parse_let () =
  check exp "declaration applies"
  (parse_exp (`List [`Atom "let" ; `List [`Atom "x" ; `Atom "1"] ; `List [`Atom "let" ; `List [`Atom "y" ; `Atom "7"] ; `Atom "10"] ])) 
  (Let ("x", Num (1L,-1), Let ("y", Num (7L,-1), Num (10L,-1),-1),-1))

let test_parse_compound () =
  check exp "same expr"
    (parse_exp (`List [`Atom "+" ; `List [`Atom "+" ; `Atom "3"; `Atom "x"]; `Atom "7"]))
    (Prim2 (Add, Prim2 (Add, Num (3L,-1), Id ("x",-1),-1), Num (7L,-1),-1))

(* let test_parse_error () =
  let sexp = `List [`Atom "foo"; `Atom "bar"] in
  check_raises "Should raise failwith" 
    (Failure (Fmt.str "Not a valid expr: %a" CCSexp.pp sexp))
    (fun () -> ignore @@ parse_exp sexp) *)

(* Tests for our [interp] function *)

let test_parse_error () =
  let sexp = `List [`List [`Atom "foo"]; `Atom "bar"] in
  check_raises "Should raise a parse error" 
    (CTError (Fmt.str "Not a valid expr: %a" CCSexp.pp sexp))
    (fun () -> ignore @@ parse_exp sexp)

(* Tests for our [interp] function *)
let test_interp_num () =
  check value "same int" (interp (Num (42L,-1)) empty_env empty_fenv) (NumV (42L))

let test_interp_var () =
  check value "same int" (interp (Id ("x",-1)) (extend_env ["x"] [(NumV (7L))] empty_env) empty_fenv) (NumV (7L))

let test_interp_bool () =
  let v = (interp (Bool (true,-1)) empty_env empty_fenv) in check value "same bool" v 
  (BoolV true)

let test_interp_add1 () =
  let v = (interp (Prim1 (Add1, Num (1L,-1),-1)) empty_env empty_fenv) in 
  check value "correct increment" v 
  (NumV 2L)

let test_interp_sub1 () =
  let v = (interp (Prim1 (Sub1, Num (1L,-1),-1)) empty_env empty_fenv) in 
  check value "correct decrement" v 
  (NumV 0L)

let test_interp_add () =
  let v = (interp (Prim2 (Add, Num (1L,-1), Num ((-1L),-1),-1)) empty_env empty_fenv) in 
  check value "correct addition" v 
  (NumV 0L)

let test_interp_less () =
  let v = (interp (Prim2 (Lte, Num (70L,-1), Num (10L,-1),-1))) empty_env empty_fenv in 
  check value "correct lesser comparison" v 
  (BoolV false)

let test_interp_and () =
  let v = (interp (Prim2 (And, Bool (true,-1), Bool (false,-1),-1))) empty_env empty_fenv in 
  check value "correct conjunction" v 
  (BoolV false)

let test_interp_fork_1 () =
  let v = (interp (If (Prim2 (Lte, Num (0L,-1), Num (1L,-1),-1), (Num (70L,-1)), (Bool (false,-1)),-1))) empty_env empty_fenv in 
  check value "correct execution fork true" v 
  (NumV 70L)
  
let test_interp_fork_2 () =
  let v = (interp (If (Prim2 (Lte, Num (1L,-1), Num (0L,-1),-1), (Num (70L,-1)), (Bool (false,-1)),-1))) empty_env empty_fenv in 
  check value "correct execution fork false" v 
  (BoolV false)
  
let test_interp_let_1 () =
  let v = (interp (Let ("x", Num (1L,-1), (Let ("y", Bool (false,-1), (Prim2 (And, Id ("y",-1), (Prim2 (Lte, Id ("x",-1), Num (0L,-1),-1)),-1)),-1)),-1))) empty_env empty_fenv in
  check value "correct simple variable assignment" v 
  (BoolV false)
  
let test_interp_let_2 () =
  let v = (interp (Let ("x", Num (2L,-1), (Let ("y", (Let ("x", Num (1L,-1), Id ("x",-1),-1)), (Prim2 (Add, Id ("x",-1), Id ("x",-1),-1)),-1)),-1))) empty_env empty_fenv in
  check value "correct complex variable assignment" v 
  (NumV 4L)

let test_interp_fo_fun_1 () =
  let v = (interp_prog (
    [DefFun ("f", ["x"], (Prim2 (Add, Id ("x",-1), Id ("x",-1),-1)))],
    (Apply ("f", [Num (2L,-1)],-1))
  )) empty_env in 
  check value "correct simple function execution" v 
  (NumV 4L)

let test_interp_tup () =
  let v = (interp (Tuple ([Num (12L,-1);Bool(true,-1);Tuple ([],-1)],-1))) empty_env empty_fenv in
  check value "a tuple val" v 
  (TupleV [(ref (NumV 12L));(ref (BoolV true)); (ref (TupleV []))])

let test_interp_empty_tup () =
  let v = (interp (Tuple ([],-1))) empty_env empty_fenv in
  check value "an empty tuple val" v 
  (TupleV [])

let test_interp_get () =
  let v = (interp (Prim2 (Get ,Tuple ([Num(12L,-1);Bool(true,-1);Tuple ([],-1)],-1),Num (0L,-1),-1))) empty_env empty_fenv in
  check value "correct get execution" v 
  (NumV 12L)

let test_interp_set () =
  let v = (interp (Let ("x",(Tuple ([Num (12L,-1);Bool(true,-1);Tuple ([],-1)],-1)) , (Let ("foo", (Set (Id ("x",-1), Num (1L,-1), Bool(false,-1),-1)), (Id ("x",-1)),-1)),-1))) empty_env empty_fenv in
  check value "correct set execution" v 
  (TupleV [(ref (NumV 12L));(ref (BoolV false)); (ref (TupleV []))])
  
let test_interp_fo_fun_2 () =
  let v = (interp_prog (
    [DefFun ("f", ["x" ; "y" ; "z"], (Prim2 (Add, (Prim2 (Add, Id ("x",-1), Id ("y",-1),-1)), Id ("z",-1),-1)))],
  (Apply ("f" , [Num (2L,-1) ; Num (20L,-1) ; Num (200L,-1)],-1))
  )) empty_env in 
  check value "correct complex function execution" v 
  (NumV 222L)
  
let test_interp_fo_app_1 () =
  check value "correct simple function application"
  (NumV 14L)
  (interp_prog ( 
  (
    [
      DefFun ("f", ["x" ; "y"], (Prim2 (Add, Id ("x",-1), Id ("y",-1),-1)));
      DefFun ("g", ["y"], (Prim2 (Add, Num (7L,-1), Id ("y",-1),-1)))
    ],
    (Apply ("g", [
      (Apply ("f", [Num (4L,-1) ; Num (3L,-1)],-1))
    ],-1))
  )
  ) empty_env)

let test_interp_fo_app_2 () =
  check value "correct simple function application"
  (NumV 200L)
  (interp_prog ( 
  (
    [
      DefFun ("f", ["x" ; "y"], (Prim2 (Add, Id ("x",-1), Id ("y",-1),-1)));
      DefFun ("g", ["x"], (Apply ("f", [Id ("x",-1) ; Id ("x",-1)],-1)))
    ],
    (Apply ("g", [Num (100L,-1)],-1))
  )
  ) empty_env)

let test_interp_compound () =
  check value "same int"
    (interp (Prim2 (Add, Prim2 (Add, Num (3L,-1), (Prim1 (Sub1, Num (6L,-1),-1)),-1), Num (12L,-1),-1)) empty_env empty_fenv)
    (NumV 20L)

let lazy_and () =
  check value "reduces to false without throwing an error or printing"
  (interp (parse_exp (sexp_from_string "(and false (print -1))")) empty_env empty_fenv)
  (BoolV false)

let error_if_cond_not_bool () =
  let v = (fun () -> ignore @@ interp (parse_exp (sexp_from_string "(if 23 true false)")) empty_env empty_fenv) in 
  check_raises "if received a non-boolean as condition" 
  (RTError "Type error: Expected boolean but got 23") v

let test_error_III () =
  let v = (fun () -> ignore @@ interp (Prim2 (Add, Bool (true,-1),  Num (-1L,-1),-1)) empty_env empty_fenv) in 
  check_raises "incorrect addition" 
  (RTError "Type error: Expected integer but got true") v


let test_error_BBB () =
  let v = (fun () -> ignore @@ (interp (Prim2 (And, Num (5L,-1), Bool (false,-1),-1))) empty_env empty_fenv) in 
  check_raises "incorrect conjunction"
  (RTError "Type error: Expected boolean but got 5") v

let test_error_IIB () =
  let v = (fun () -> ignore @@ (interp (Prim2 (Lte, Bool (true,-1), Num (10L,-1),-1))) empty_env empty_fenv) in 
  check_raises  "incorrect lesser comparison" 
  (RTError "Type error: Expected integer but got true") v

(* When there is more than one operand with the wrong type, report the first one. *)
let test_type_error_mult_illtyped () =
  let v = (fun () -> ignore @@ (interp (Prim2 (Add, Bool(true,-1), Bool(false,-1),-1))) empty_env empty_fenv) in 
  check_raises  "should report first ill-typed argument" 
  (RTError "Type error: Expected integer but got true") v

(* Arity mismatch and undefined function errors *)
(*  All these tests use the same function definitions, but with different expressions in <body>
      ( 
        (def (g x) (if (<= x 5) 5 x))
        (def (f x y) (+ (+ 2 x) (g y)))
        <body>
      )
    When,
      <body> := (f 8)      =>  "Arity mismatch: f expected 2 arguments but got 1"
      <body> := (g 4 5 6)  =>  "Arity mismatch: g expected 1 arguments but got 3"
      <body> := (h 1)      =>  "Undefined function: h"
*)
let test_error_arity_less_args () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (def (g x) (if (<= x 5) 5 x))
                      (def (f x y) (+ (+ 2 x) (g y)))
                      (f 8))")))
                    empty_env) in 
  check_raises  "should report arity mismatch (1 arg instead of 2)" 
  (CTError "Arity mismatch: f expected 2 arguments but got 1") v

let test_error_arity_more_args () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (def (g x) (if (<= x 5) 5 x))
                      (def (f x y) (+ (+ 2 x) (g y)))
                      (g 4 5 6))")))
                    empty_env) in 
  check_raises  "should report arity mismatch (3 args instead of 1)" 
  (CTError "Arity mismatch: g expected 1 arguments but got 3") v

let test_error_undef_h () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (def (g x) (if (<= x 5) 5 x))
                      (def (f x y) (+ (+ 2 x) (g y)))
                      (h 1))")))
                    empty_env) in 
  check_raises  "should report undefined fun `h`" 
  (CTError "Undefined function: h") v

(* Foreign functions: Type errors and arity mismatches *)
(*  All these tests use the same FF declarations, but with different expressions in <body>
      ( 
        (defsys print any -> any)
        (defsys max int int -> int)
        (defsys xor bool bool -> bool)
        <body>
      )
    When,
      <body> := (max 5)          =>  "Arity mismatch: max expected 2 arguments but got 1"
      <body> := (print 8 false)  =>  "Arity mismatch: print expected 1 arguments but got 2"
      <body> := (xor true 23)    =>  "Type error: Expected boolean but got 23"
      <body> := (xor -35 23)     =>  "Type error: Expected boolean but got -35"
      <body> := (pow 3 4)        =>  "Undefined function: pow"
*)
let test_ffi_error_arity_less_args () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (defsys print any -> any)
                      (defsys max int int -> int)
                      (defsys xor bool bool -> bool)
                      (max 5))")))
                    empty_env) in 
  check_raises  "should report arity mismatch (1 arg instead of 2)" 
  (CTError "Arity mismatch: max expected 2 arguments but got 1") v

let test_ffi_error_arity_more_args () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (defsys print any -> any)
                      (defsys max int int -> int)
                      (defsys xor bool bool -> bool)
                      (print 8 false))")))
                    empty_env) in 
  check_raises  "should report arity mismatch (2 args instead of 1)" 
  (CTError "Arity mismatch: print expected 1 arguments but got 2") v

let test_ffi_type_error () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (defsys print any -> any)
                      (defsys max int int -> int)
                      (defsys xor bool bool -> bool)
                      (xor true 23))")))
                    empty_env) in 
  check_raises  "should report type error" 
  (RTError "Type error: Expected boolean but got 23") v

let test_ffi_error_multi_ill_typed () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (defsys print any -> any)
                      (defsys max int int -> int)
                      (defsys xor bool bool -> bool)
                      (xor -35 23))")))
                    empty_env) in 
  check_raises  "should report the first ill-typed value" 
  (RTError "Type error: Expected boolean but got -35") v

let test_ffi_error_undef_pow () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (defsys print any -> any)
                      (defsys max int int -> int)
                      (defsys xor bool bool -> bool)
                      (pow 3 4))")))
                    empty_env) in 
  check_raises  "should report that `pow` is undefined" 
  (CTError "Undefined function: pow") v

(* Tuple errors: type errors produced by ill-typed arguments to [get] and [set],
   and index-out-of-bounds errors. *)

let test_get_error_ill_typed_tuple () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (get false 12))")))
                    empty_env) in 
  check_raises  "should report that first argument is not a tuple" 
  (RTError "Type error: Expected tuple but got false") v

let test_get_error_ill_typed_index () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (get (tup 2 true) (tup)))")))
                    empty_env) in 
  check_raises  "should report that second argument is not an integer" 
  (RTError "Type error: Expected integer but got (tup)") v

let test_get_error_multi_ill_typed () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (get -34 true))")))
                    empty_env) in 
  check_raises  "should report the first ill-typed argument" 
  (RTError "Type error: Expected tuple but got -34") v

let test_get_index_out_of_bounds () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (get (tup 2 true (tup)) -4))")))
                    empty_env) in 
  check_raises  "should report index -4 out of bounds" 
  (RTError "Index out of bounds: Tried to access index -4 of (tup 2 true (tup))") v


let test_set_error_ill_typed_tuple () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (set false 12 true))")))
                    empty_env) in 
  check_raises  "should report that first argument is not a tuple" 
  (RTError "Type error: Expected tuple but got false") v

let test_set_error_ill_typed_index () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (set (tup 2 true) (tup) -23))")))
                    empty_env) in 
  check_raises  "should report that second argument is not an integer" 
  (RTError "Type error: Expected integer but got (tup)") v

let test_set_error_multi_ill_typed () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (set -34 true (tup)))")))
                    empty_env) in 
  check_raises  "should report the first ill-typed argument" 
  (RTError "Type error: Expected tuple but got -34") v

let test_set_index_out_of_bounds () =
  let v = (fun () -> ignore @@
                    (interp_prog (parse_prog (sexp_from_string "( 
                      (set (tup 2 true (tup)) 3 true))")))
                    empty_env) in 
  check_raises  "should report index 3 out of bounds" 
  (RTError "Index out of bounds: Tried to access index 3 of (tup 2 true (tup))") v

(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "parse", [
    test_case "A number" `Quick test_parse_int ;
    test_case "A variable" `Quick test_parse_var ;
    test_case "A boolean" `Quick test_parse_bool ;
    test_case "A tuple" `Quick test_parse_tuple ;
    test_case "An increment" `Quick test_parse_add1 ;
    test_case "A decrement" `Quick test_parse_sub1 ;
    test_case "An addition" `Quick test_parse_add ;
    test_case "A get" `Quick test_parse_get ;
    test_case "An set" `Quick test_parse_set ;
    test_case "A lesser comparison" `Quick test_parse_less ;
    test_case "A conjunction" `Quick test_parse_and ;
    test_case "An if clause" `Quick test_parse_fork ;
    test_case "A definition" `Quick test_parse_let ;
    test_case "A compound expression" `Quick test_parse_compound ;
    (* test_case "An invalid s-expression" `Quick test_parse_error *)
  ] ;
  "interp", [
    test_case "A number" `Quick test_interp_num ;
    test_case "A variable" `Quick test_interp_var ;
    test_case "A boolean" `Slow test_interp_bool ;
    test_case "A tuple" `Slow test_interp_tup ;
    test_case "An increment" `Slow test_interp_add1 ;
    test_case "An decrement" `Slow test_interp_sub1 ;
    test_case "An addition" `Slow test_interp_add ;
    test_case "A lesser comparison" `Slow test_interp_less ;
    test_case "A conjunction" `Slow test_interp_and ;
    test_case "An if clause when true" `Slow test_interp_fork_1 ;
    test_case "An if clause when false" `Slow test_interp_fork_2 ;
    test_case "A simple definition" `Slow test_interp_let_1 ;
    test_case "A complex definition" `Slow test_interp_let_2 ;
    test_case "A simple function" `Slow test_interp_fo_fun_1 ;
    test_case "A complex function" `Slow test_interp_fo_fun_2 ;
    test_case "A simple application" `Slow test_interp_fo_app_1 ;
    test_case "A complex application" `Slow test_interp_fo_app_2 ;
    test_case "A compound expression" `Quick test_interp_compound;
    test_case "A get expression" `Slow test_interp_get ;
    test_case "A set expression" `Slow test_interp_set ;
    (* test_case "`and` is lazy" `Quick lazy_and *)
  ] ;
  "errors", [
    test_case "Addition of true" `Quick test_error_III ;
    test_case "And of 5" `Quick test_error_BBB ;
    test_case "If received non-boolean condition" `Quick error_if_cond_not_bool ;
    test_case "Lesser than true" `Quick test_error_IIB ;
    test_case "Report first ill-typed argument" `Quick test_type_error_mult_illtyped ;
    test_case "Arity mismatch: less args" `Quick test_error_arity_less_args ;
    test_case "Arity mismatch: more args" `Quick test_error_arity_more_args ;
    test_case "Undefined function `h`" `Quick test_error_undef_h ;

    test_case "FF Arity mismatch: `max` received less args" `Quick test_ffi_error_arity_less_args ;
    test_case "FF Arity mismatch: `print` received more args" `Quick test_ffi_error_arity_more_args ;
    test_case "FF Type error: apply `xor` to 23 " `Quick test_ffi_type_error ;
    test_case "FF Report first ill-typed arg to `xor`" `Quick test_ffi_error_multi_ill_typed ;
    test_case "FF Undefined: `pow`" `Quick test_ffi_error_undef_pow ;

    test_case "Tuple Get: Ill-typed tuple" `Quick test_get_error_ill_typed_tuple ;
    test_case "Tuple Get: Ill-typed index" `Quick test_get_error_ill_typed_index ;
    test_case "Tuple Get: Multiple ill-typed arguments" `Quick test_get_error_multi_ill_typed;
    test_case "Tuple Get: Index out of bounds" `Quick test_get_index_out_of_bounds ;

    test_case "Tuple Set: Ill-typed tuple" `Quick test_set_error_ill_typed_tuple ;
    test_case "Tuple Set: Ill-typed index" `Quick test_set_error_ill_typed_index ;
    test_case "Tuple Set: Multiple ill-typed arguments" `Quick test_set_error_multi_ill_typed;
    test_case "Tuple Set: Index out of bounds" `Quick test_set_index_out_of_bounds ;

  ]
]     

(* Entry point of tester *)
let () =
  (* BBC tests: don't change the following, simply add .bbc files in the bbctests/ directory *)
  let bbc_tests = 
    let compile_flags = Option.value (Sys.getenv_opt "CFLAGS") ~default:"-g" in
    let compiler : string -> out_channel -> unit = 
      fun s o -> fprintf o "%s" (compile_prog (parse_prog (sexp_from_string s))) in
    let oracle : string -> status * string = (
      fun s -> (
        try
          NoError, string_of_val (interp_prog (parse_prog (sexp_from_string s)) empty_env)
        with
        | RTError msg -> RTError, msg
        | CTError msg -> CTError, msg
        |  e -> RTError, "Oracle raised an unknown error :"^ Printexc.to_string e 
      )
    ) in
    tests_from_dir ~compile_flags ~compiler ~oracle ~runtime:"rt/sys.c" "bbctests" in
  run "Tests entrega 1" (ocaml_tests @ bbc_tests)
