[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Interpret_lib

let eval ?(steps = 10_000) src =
  match Interpret.run ~steps src with
  | Ok (v, output) ->
    List.iter print_endline output;
    print_endline (Interpret.format_value v)
  | Error e -> print_endline (Interpret.format_error e)
;;

let%expect_test "integer literal" =
  eval "42";
  [%expect {| 42 |}]
;;

let%expect_test "arithmetic precedence" =
  eval "2 + 3 * 4";
  [%expect {| 14 |}]
;;

let%expect_test "parenthesised expression" =
  eval "(2 + 3) * 4";
  [%expect {| 20 |}]
;;

let%expect_test "unary minus" =
  eval "-5 + 2";
  [%expect {| -3 |}]
;;

let%expect_test "comparisons yield ints" =
  eval "3 < 5";
  [%expect {| 1 |}];
  eval "5 < 3";
  [%expect {| 0 |}]
;;

let%expect_test "neq operator" =
  eval "3 <> 5";
  [%expect {| 1 |}];
  eval "3 <> 3";
  [%expect {| 0 |}]
;;

let%expect_test "le operator" =
  eval "3 <= 5";
  [%expect {| 1 |}];
  eval "5 <= 3";
  [%expect {| 0 |}]
;;

let%expect_test "gt operator" =
  eval "5 > 3";
  [%expect {| 1 |}];
  eval "3 > 5";
  [%expect {| 0 |}]
;;

let%expect_test "ge operator" =
  eval "5 >= 5";
  [%expect {| 1 |}];
  eval "3 >= 5";
  [%expect {| 0 |}]
;;

let%expect_test "let binding" =
  eval "let x = 10 in x + 7";
  [%expect {| 17 |}]
;;

let%expect_test "shadowing" =
  eval "let x = 1 in let x = 2 in x + 10";
  [%expect {| 12 |}]
;;

let%expect_test "closures capture environment" =
  eval {|
    let x = 10 in
    let f = fun y -> x + y in
    let x = 100 in
    f 5
  |};
  [%expect {| 15 |}]
;;

let%expect_test "lambda application" =
  eval "(fun x -> x + 1) 5";
  [%expect {| 6 |}]
;;

let%expect_test "multi-argument sugar" =
  eval "let add = fun x y -> x + y in add 3 4";
  [%expect {| 7 |}]
;;

let%expect_test "partial application" =
  eval "let add x y = x + y in let add5 = add 5 in add5 3 + add5 2";
  [%expect {| 15 |}]
;;

let%expect_test "higher-order function" =
  eval "let apply_twice f x = f (f x) in let inc x = x + 1 in apply_twice inc 0";
  [%expect {| 2 |}]
;;

let%expect_test "returning a function prints <fun>" =
  eval "fun x -> x";
  [%expect {| <fun> |}]
;;

let%expect_test "factorial" =
  eval "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5";
  [%expect {| 120 |}]
;;

let%expect_test "fibonacci" =
  eval
    {|
    let rec fib n =
      if n = 0 then 0
      else if n = 1 then 1
      else fib (n - 1) + fib (n - 2)
    in fib 10
  |};
  [%expect {| 55 |}]
;;

let%expect_test "print side-effect and value" =
  eval "let _ = print 42 in 0";
  [%expect {|
    42
    0 |}]
;;

let%expect_test "multiple prints" =
  eval "let _ = print 1 in let _ = print 2 in 3";
  [%expect {|
    1
    2
    3 |}]
;;

let%expect_test "infinite loop hits step limit" =
  eval ~steps:50 "let rec loop x = loop x in loop 1";
  [%expect {| Error: step limit exceeded |}]
;;

let%expect_test "factorial with tight limit" =
  eval ~steps:20 "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 10";
  [%expect {| Error: step limit exceeded |}]
;;

let%expect_test "factorial with enough steps" =
  eval ~steps:5000 "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 10";
  [%expect {| 3628800 |}]
;;

let%expect_test "unbound variable" =
  eval "x + 1";
  [%expect {| Error: unbound variable x |}]
;;

let%expect_test "not a function" =
  eval "5 3";
  [%expect {| Error: not a function |}]
;;

let%expect_test "division by zero" =
  eval "10 / 0";
  [%expect {| Error: division by zero |}]
;;

let%expect_test "if condition is not int" =
  eval "if fun x -> x then 1 else 2";
  [%expect {| Error: expected an integer |}]
;;

let%expect_test "binop on non-int" =
  eval "1 + (fun x -> x)";
  [%expect {| Error: expected an integer |}]
;;

let%expect_test "print non-int" =
  eval "print (fun x -> x)";
  [%expect {| Error: expected an integer |}]
;;

let%expect_test "let rec with non-function" =
  eval "let rec x = 1 in x";
  [%expect {| Error: let rec requires a function on the right-hand side |}]
;;

let%expect_test "parse error" =
  eval "let x = in 5";
  [%expect {| Parse error |}]
;;

let%expect_test "pp_value for integers" =
  Format.printf "%a" Interpret.pp_value (Interpret.VInt 99);
  [%expect {| 99 |}]
;;

let%expect_test "pp_value for closure" =
  Format.printf
    "%a"
    Interpret.pp_value
    (Interpret.VClosure ("x", Ast.EVar "x", Interpret.Env.empty));
  [%expect {| <closure> |}]
;;

let%expect_test "pp_value for builtin" =
  Format.printf "%a" Interpret.pp_value (Interpret.VBuiltin "print");
  [%expect {| <builtin:print> |}]
;;

let%expect_test "format_value for closure" =
  print_endline
    (Interpret.format_value (Interpret.VClosure ("x", Ast.EVar "x", Interpret.Env.empty)));
  [%expect {| <fun> |}]
;;

let%expect_test "format_value for builtin" =
  print_endline (Interpret.format_value (Interpret.VBuiltin "print"));
  [%expect {| <fun> |}]
;;

let%expect_test "pprint negative constant" =
  print_endline (Pprint.to_string (Ast.EConst (-5)));
  [%expect {| (-5) |}]
;;
