[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "*/"]

open Mml.Parser
open Mml.Interpret

let run_eval str =
  match parse str with
  | Ok ast ->
    (match run 10000 ast with
     | Ok n -> Printf.printf "%d" n
     | Error `Division_by_zero -> print_string "Error: Division by zero"
     | Error (`UnknownVariable v) -> Printf.printf "Error: Unbound variable: %s" v
     | Error (`Type_error msg) -> Printf.printf "Error: %s" msg
     | Error `Steps_exceeded -> print_string "Error: Steps exceeded")
  | Error (`Parsing_error msg) -> Printf.printf "Parse error: %s" msg
;;

(* Basic integer tests *)
let%expect_test "evaluate integer 123" =
  run_eval "123";
  [%expect {| 123 |}]
;;

let%expect_test "evaluate zero" =
  run_eval "0";
  [%expect {| 0 |}]
;;

let%expect_test "evaluate negative integer" =
  run_eval "-17";
  [%expect {| -17 |}]
;;

(* Arithmetic operations *)
let%expect_test "evaluate addition 5 plus 8" =
  run_eval "5 + 8";
  [%expect {| 13 |}]
;;

let%expect_test "evaluate subtraction" =
  run_eval "20 - 7";
  [%expect {| 13 |}]
;;

let%expect_test "test case" =
  run_eval "6 * 9";
  [%expect {| 54 |}]
;;

let%expect_test "test case" =
  run_eval "45 / 9";
  [%expect {| 5 |}]
;;

let%expect_test "test case" =
  run_eval "10 + 5 * 2";
  [%expect {| 20 |}]
;;

let%expect_test "test case" =
  run_eval "(10 + 5) * 2";
  [%expect {| 30 |}]
;;

(* Comparison tests *)
let%expect_test "test case" =
  run_eval "3 < 7";
  [%expect {| 1 |}]
;;

let%expect_test "interpreter test 1" =
  run_eval "9 < 5";
  [%expect {| 0 |}]
;;

let%expect_test "interpreter test 2" =
  run_eval "4 <= 4";
  [%expect {| 1 |}]
;;

let%expect_test "interpreter test 3" =
  run_eval "7 = 7";
  [%expect {| 1 |}]
;;

let%expect_test "evaluate equality false" =
  run_eval "3 = 5";
  [%expect {| 0 |}]
;;

let%expect_test "interpreter test 5" =
  run_eval "12 >= 8";
  [%expect {| 1 |}]
;;

let%expect_test "interpreter test 6" =
  run_eval "6 > 9";
  [%expect {| 0 |}]
;;

(* Let binding tests *)
let%expect_test "interpreter test 7" =
  run_eval "let a = 7 in a + 3";
  [%expect {| 10 |}]
;;

let%expect_test "interpreter test 8" =
  run_eval "let x = 4 in let y = 6 in x * y";
  [%expect {| 24 |}]
;;

let%expect_test "interpreter test 9" =
  run_eval "let z = 3 * 5 in z - 2";
  [%expect {| 13 |}]
;;

(* Conditional tests *)
let%expect_test "interpreter test 10" =
  run_eval "if 8 > 3 then 111 else 222";
  [%expect {| 111 |}]
;;

let%expect_test "interpreter test 11" =
  run_eval "if 2 < 1 then 111 else 222";
  [%expect {| 222 |}]
;;

let%expect_test "interpreter test 12" =
  run_eval "if 0 then 7 else 9";
  [%expect {| 9 |}]
;;

let%expect_test "interpreter test 13" =
  run_eval "if 55 then 7 else 9";
  [%expect {| 7 |}]
;;

(* Lambda and application tests *)
let%expect_test "cannot evaluate lambda without application" =
  run_eval "(fun x -> x + 3) 7";
  [%expect {| 10 |}]
;;

let%expect_test "interpreter test 15" =
  run_eval "(fun a -> fun b -> a * b) 5 6";
  [%expect {| 30 |}]
;;

let%expect_test "interpreter test 16" =
  run_eval "let triple = fun n -> n * 3 in triple 14";
  [%expect {| 42 |}]
;;

(* Recursive function tests *)
let%expect_test "interpreter test 17" =
  run_eval "let rec f n = if n <= 1 then 1 else n * f (n - 1) in f 6";
  [%expect {| 720 |}]
;;

let%expect_test "interpreter test 18" =
  run_eval "let rec f n = if n <= 1 then 1 else n * f (n - 1) in f 1";
  [%expect {| 1 |}]
;;

let%expect_test "interpreter test 19" =
  run_eval "let rec g n = if n <= 1 then n else g (n-1) + g (n-2) in g 7";
  [%expect {| 13 |}]
;;

let%expect_test "interpreter test 20" =
  run_eval "let rec g n = if n <= 1 then n else g (n-1) + g (n-2) in g 1";
  [%expect {| 1 |}]
;;

(* Combined complex expressions *)
let%expect_test "interpreter test 21" =
  run_eval "5 * 4 + 30 / (10 - 5)";
  [%expect {| 26 |}]
;;

let%expect_test "interpreter test 22" =
  run_eval "let p = 3 in let q = 5 in let r = 7 in p * q + r";
  [%expect {| 22 |}]
;;

(* Error handling tests *)
let%expect_test "interpreter test 23" =
  run_eval "unknown_var";
  [%expect {| Error: Unbound variable: unknown_var |}]
;;

let%expect_test "interpreter test 24" =
  run_eval "25 / 0";
  [%expect {| Error: Division by zero |}]
;;

let%expect_test "interpreter test 25" =
  run_eval "99 42";
  [%expect {| Error: applying non-function |}]
;;

(* Step limit tests *)
let run_eval_limited str limit =
  match parse str with
  | Ok ast ->
    (match run limit ast with
     | Ok n -> Printf.printf "%d" n
     | Error `Division_by_zero -> print_string "Error: Division by zero"
     | Error (`UnknownVariable v) -> Printf.printf "Error: Unbound variable: %s" v
     | Error (`Type_error msg) -> Printf.printf "Error: %s" msg
     | Error `Steps_exceeded -> print_string "Error: Steps exceeded")
  | Error (`Parsing_error msg) -> Printf.printf "Parse error: %s" msg
;;

let%expect_test "step limit exceeded on infinite loop" =
  run_eval_limited "let rec loop n = loop (n + 1) in loop 0" 100;
  [%expect {| Error: Steps exceeded |}]
;;

let%expect_test "interpreter test 27" =
  run_eval_limited "let rec f n = if n <= 0 then 0 else f (n-1) + f (n-1) in f 20" 1000;
  [%expect {| Error: Steps exceeded |}]
;;

let%expect_test "interpreter test 28" =
  run_eval_limited "2 + 3" 5;
  [%expect {| 5 |}]
;;
