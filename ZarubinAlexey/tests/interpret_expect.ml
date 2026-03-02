(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Base

(* Печатаем результат так же, как делает REPL:
   сначала строки, накопленные через print
   потом итоговое число
   при ошибке печатаем стабильное сообщение *)
let print_run (s : string) =
  match Lambda_lib.Interpret.run_string s with
  | Ok (n, out) ->
    List.iter out ~f:Stdlib.print_endline;
    Stdlib.print_endline (Int.to_string n)
  | Error (`Parsing_error _) -> Stdlib.print_endline "Parsing error"
  | Error (`UnknownVariable x) ->
    Stdlib.print_endline ("Interpreter error: unknown variable " ^ x)
  | Error `IfConditionNotInt ->
    Stdlib.print_endline "Interpreter error: if condition is not int"
  | Error `BinopOnNonInt -> Stdlib.print_endline "Interpreter error: binop on non-int"
  | Error `NotAFunction -> Stdlib.print_endline "Interpreter error: not a function"
  | Error `DivisionByZero -> Stdlib.print_endline "Interpreter error: division by zero"
  | Error `ResultNotInt -> Stdlib.print_endline "Interpreter error: result is not int"
  | Error `FixOnNonFunction ->
    Stdlib.print_endline "Interpreter error: fix on non-function"
  | Error `PrintArgumentNotInt ->
    Stdlib.print_endline "Interpreter error: print argument not int"
  | Error `StepLimitExceeded ->
    Stdlib.print_endline "Interpreter error: step limit exceeded"
;;

let print_run_steps ~(steps : int) (s : string) =
  match Lambda_lib.Interpret.run_string_with_steps ~steps s with
  | Ok (n, out) ->
    List.iter out ~f:Stdlib.print_endline;
    Stdlib.print_endline (Int.to_string n)
  | Error (`Parsing_error _) -> Stdlib.print_endline "Parsing error"
  | Error (`UnknownVariable x) ->
    Stdlib.print_endline ("Interpreter error: unknown variable " ^ x)
  | Error `IfConditionNotInt ->
    Stdlib.print_endline "Interpreter error: if condition is not int"
  | Error `BinopOnNonInt -> Stdlib.print_endline "Interpreter error: binop on non-int"
  | Error `NotAFunction -> Stdlib.print_endline "Interpreter error: not a function"
  | Error `DivisionByZero -> Stdlib.print_endline "Interpreter error: division by zero"
  | Error `ResultNotInt -> Stdlib.print_endline "Interpreter error: result is not int"
  | Error `FixOnNonFunction ->
    Stdlib.print_endline "Interpreter error: fix on non-function"
  | Error `PrintArgumentNotInt ->
    Stdlib.print_endline "Interpreter error: print argument not int"
  | Error `StepLimitExceeded ->
    Stdlib.print_endline "Interpreter error: step limit exceeded"
;;

let%expect_test "basic/int" =
  print_run "5";
  [%expect {| 5 |}]
;;

let%expect_test "arithmetic/precedence" =
  print_run "2 + 3 * 4";
  [%expect {| 14 |}]
;;

let%expect_test "arithmetic/parentheses" =
  print_run "(2 + 3) * 4";
  [%expect {| 20 |}]
;;

let%expect_test "unary minus" =
  print_run "-5 + 2";
  [%expect {| -3 |}]
;;

let%expect_test "let" =
  print_run "let x = 10 in x + 7";
  [%expect {| 17 |}]
;;

let%expect_test "shadowing" =
  print_run "let x = 1 in let x = 2 in x + 10";
  [%expect {| 12 |}]
;;

let%expect_test "functions/currying" =
  print_run "let add = fun x y -> x + y in add 3 4";
  [%expect {| 7 |}]
;;

let%expect_test "closures (lexical scoping)" =
  print_run
    {|
      let x = 10 in
      let f = fun y -> x + y in
      let x = 100 in
      f 5
    |};
  [%expect {| 15 |}]
;;

let%expect_test "if on ints" =
  print_run "if 0 then 111 else 222";
  [%expect {| 222 |}]
;;

let%expect_test "comparisons" =
  print_run "if 3 > 2 then 1 else 0";
  [%expect {| 1 |}]
;;

let%expect_test "recursion/factorial" =
  print_run "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5";
  [%expect {| 120 |}]
;;

let%expect_test "recursion/fibonacci" =
  print_run
    {|
      let rec fib n =
        if n = 0 then 0
        else if n = 1 then 1
        else fib (n - 1) + fib (n - 2)
      in fib 10
    |};
  [%expect {| 55 |}]
;;

let%expect_test "print side effect" =
  print_run "let _ = print 10 in 5";
  [%expect {|
    10
    5 |}]
;;

(* Ошибки *)

let%expect_test "parser error" =
  print_run "let x = in 5";
  [%expect {| Parsing error |}]
;;

let%expect_test "unknown variable" =
  print_run "x + 1";
  [%expect {| Interpreter error: unknown variable x |}]
;;

let%expect_test "not a function" =
  print_run "(5) 3";
  [%expect {| Interpreter error: not a function |}]
;;

let%expect_test "division by zero" =
  print_run "10 / 0";
  [%expect {| Interpreter error: division by zero |}]
;;

let%expect_test "non-integer result" =
  print_run "fun x -> x";
  [%expect {| Interpreter error: result is not int |}]
;;

let%expect_test "binop on non-int" =
  print_run "1 + (fun x -> x)";
  [%expect {| Interpreter error: binop on non-int |}]
;;

let%expect_test "bad if condition" =
  print_run "if fun x -> x then 1 else 2";
  [%expect {| Interpreter error: if condition is not int |}]
;;

let%expect_test "print arg not int" =
  print_run "print (fun x -> x)";
  [%expect {| Interpreter error: print argument not int |}]
;;

(* Шаги *)

let%expect_test "steps: infinite recursion hits limit" =
  print_run_steps ~steps:50 "let rec f x = f x in f 1";
  [%expect {| Interpreter error: step limit exceeded |}]
;;

let%expect_test "steps: factorial fails with too small limit" =
  print_run_steps
    ~steps:20
    "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 10";
  [%expect {| Interpreter error: step limit exceeded |}]
;;

let%expect_test "steps: factorial succeeds with enough limit" =
  print_run_steps
    ~steps:5000
    "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 10";
  [%expect {| 3628800 |}]
;;

let%expect_test "steps: print and computation still consume steps" =
  print_run_steps ~steps:50 "let _ = print 1 in let _ = print 2 in 3";
  [%expect {|
    1
    2
    3 |}]
;;
