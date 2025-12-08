[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser
open Interpreter

let run_interpreter_test input =
  let program = parse_structure_items input in
  match program with
  | Error msg -> Printf.printf "Parse Error: %s\n" msg
  | Ok prog ->
    (match Interpreter.run_program 100000 prog with
     | Ok _ -> ()
     | Error err -> Printf.printf "Runtime Error: %s\n" (Interpreter.show_error err))
;;

let%expect_test "interpreter test print int simple" =
  run_interpreter_test "let () = print_int 5";
  [%expect {| 5 |}]
;;

let%expect_test "interpreter test print int expression" =
  run_interpreter_test "let () = print_int (2 + 3 * 4)";
  [%expect {| 14 |}]
;;

let%expect_test "interpreter test let binding and print" =
  run_interpreter_test "let () = let x = 10 in print_int (x * 2)";
  [%expect {| 20 |}]
;;

let%expect_test "interpreter test function definition and application" =
  run_interpreter_test "let f x y = x + y ;; \n    let () = print_int (f 3 4)";
  [%expect {| 7 |}]
;;

let%expect_test "interpreter test recursive function (factorial)" =
  run_interpreter_test
    "let rec fact n = \n\
    \    if n then n * fact (n - 1) else 1 ;; \n\
    \    let () = print_int (fact 2000)";
  [%expect {| 0 |}]
;;

let%expect_test "interpreter test recursive function (fibonacci)" =
  run_interpreter_test
    "let fib n =\n\
    \      let rec fib_iter a b count =\n\
    \        if count then fib_iter b (a + b) (count - 1)\n\
    \        else a\n\
    \      in\n\
    \      fib_iter 0 1 n\n\
    \    ;;\n\
    \    let () = print_int (fib 9)";
  [%expect {| 34 |}]
;;

let%expect_test "lambda binding" =
  run_interpreter_test
    "let add = fun x y z -> x + y + z ;; let () = print_int (add 3 4 5)";
  [%expect {|12|}]
;;

let%expect_test "factorial with fix" =
  run_interpreter_test
    "\n\
    \    let rec fix f x = f (fix f) x ;; \n\
    \    let fact = fix (fun slf n -> if n = 0 then 1 else n * slf (n - 1)) ;;\n\
    \    let () = print_int (fact 10)\n\
    \  ";
  [%expect {| 3628800 |}]
;;

let%expect_test "functions" =
  run_interpreter_test
    "\n\
    \    let add = fun x y -> x + y ;;\n\
    \    let inc = fun x -> add x 1 ;;\n\
    \    let () = print_int (inc 41)\n\
    \  ";
  [%expect {| 42 |}]
;;

let%expect_test "let rec unit binding" =
  run_interpreter_test "let rec () = print_int 42 ;;";
  [%expect
    {| Runtime Error: Error: Type error - Recursive value definition cannot be unit |}]
;;

let%expect_test "unit function test" =
  run_interpreter_test "let rec () = print_int 42";
  [%expect
    {| Runtime Error: Error: Type error - Recursive value definition cannot be unit |}]
;;

let%expect_test "unit value test" =
  run_interpreter_test
    "let printer a b = \n\
    \    let () = print_int a in \n\
    \    let () = print_int b in\n\
    \    () ;; \n\
    \ let () = printer 42 34";
  [%expect {| 4234 |}]
;;

let%expect_test "unbound variable test" =
  run_interpreter_test "let () = print_int x";
  [%expect {| Runtime Error: Error: Unbound variable x |}]
;;

let%expect_test "unop test" =
  run_interpreter_test "let () = print_int (-5 / 2)";
  [%expect {| -2 |}]
;;

let%expect_test "binop with non-integer types" =
  run_interpreter_test "let () = print_int (5 + (fun x -> x))";
  [%expect
    {| Runtime Error: Error: Type error - Cannot apply binary operator to non-integers |}]
;;

let%expect_test "print_endl test" =
  run_interpreter_test "let () = print_endl ()";
  [%expect {| |}]
;;

let%expect_test "unit unop" =
  run_interpreter_test "let () = print_int (-())";
  [%expect {| Runtime Error: Error: Type error - Expected integer for negation |}]
;;

let%expect_test "unit condition" =
  run_interpreter_test "let () = if () then 1 else 0";
  [%expect {| Runtime Error: Error: Type error - Expected integer in if condition |}]
;;

let%expect_test "any pattern in value binding" =
  run_interpreter_test "let _ = print_int 42";
  [%expect {| 42 |}]
;;

let%expect_test "applying argument to function with no parameters" =
  run_interpreter_test "let f = 42 ;; let () = print_int (f 5)";
  [%expect {| Runtime Error: Error: Type error - Cannot apply a non-function |}]
;;

let%expect_test "equality operator test" =
  run_interpreter_test "let () = print_int (if 5 = 5 then 1 else 0)";
  [%expect {| 1 |}]
;;

let%expect_test "inequality operator test" =
  run_interpreter_test "let () = print_int (if 5 <> 3 then 1 else 0)";
  [%expect {| 1 |}]
;;

let%expect_test "less than operator test" =
  run_interpreter_test "let () = print_int (if 3 < 5 then 1 else 0)";
  [%expect {| 1 |}]
;;

let%expect_test "greater than or equal operator test" =
  run_interpreter_test "let () = print_int (if 5 >= 5 then 1 else 0)";
  [%expect {| 1 |}]
;;

let%expect_test "subtraction and multiplication" =
  run_interpreter_test "let () = print_int (10 - 2 * 3)";
  [%expect {| 4 |}]
;;

let%expect_test "division by zero" =
  run_interpreter_test "let () = print_int (10 / 0)";
  [%expect {| Runtime Error: Error: Division by zero |}]
;;

let%expect_test "remaining comparison operators" =
  run_interpreter_test
    "let () = print_int (if 5 > 4 then 1 else 0);; \n\
     let () = print_int (if 4 <= 4 then 1 else 0)";
  [%expect {| 11 |}]
;;

let%expect_test "recursive let wildcard error" =
  run_interpreter_test "let rec _ = fun x -> x";
  [%expect
    {| Runtime Error: Error: Type error - Recursive value definition cannot be a wildcard |}]
;;

let%expect_test "unit pattern mismatch in let" =
  run_interpreter_test "let () = 5";
  [%expect {| Runtime Error: Error: Type error - Expected unit value for unit pattern |}]
;;

let%expect_test "unit pattern mismatch in function application" =
  run_interpreter_test "let f () = 1 ;; let () = print_int (f 5)";
  [%expect {| Runtime Error: Error: Type error - Expected unit value for unit pattern |}]
;;

let%expect_test "wildcard argument in function" =
  run_interpreter_test "let f _ = 42 ;; let () = print_int (f 100)";
  [%expect {| 42 |}]
;;

let%expect_test "print_endl type error" =
  run_interpreter_test "let () = print_endl 5";
  [%expect {| Runtime Error: Error: Type error - print_endl expects a unit value () |}]
;;

let%expect_test "print_int type error" =
  run_interpreter_test "let () = print_int ()";
  [%expect {| Runtime Error: Error: Type error - print_int expects an integer |}]
;;

let%expect_test "let rec with non-function error" =
  run_interpreter_test "let rec f = 42";
  [%expect
    {| Runtime Error: Error: Type error - Recursive value definition must be a function |}]
;;

let%expect_test "applying argument to function with no parameters (fix)" =
  run_interpreter_test
    "let rec fix f x = f (fix f) x ;; let g = let f = fix (fun x -> 10) in f 1";
  [%expect {| Runtime Error: Error: Type error - Cannot apply a non-function |}]
;;

let%expect_test "inc" =
  run_interpreter_test
    "let inc x = \n\
    \      let add = fun x y -> x + y in\n\
    \      add x 1 ;;\n\
    \     let () = print_int (inc 41)";
  [%expect {| 42 |}]
;;
