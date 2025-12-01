[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib
open Interpret
open Parser

let parse_optimistically str = Result.get_ok (parse str)

let interpret_optimistically str steps =
  let expr = parse_optimistically str in
  Result.get_ok (run_interpret expr steps)
;;

let interpret_pessmistically str steps =
  let expr = parse_optimistically str in
  Result.get_error (run_interpret expr steps)
;;

let string_of_value = function
  | Unit -> "Unit"
  | Num n -> string_of_int n
  | Closure (v, _, _) -> "<closure>" ^ v
  | RecClosure _ -> "<rec_closure>"
;;

(* Функция для преобразования error в строку *)
let string_of_error = function
  | UnboundVariable x -> "UnboundVariable " ^ x
  | DivisionByZero -> "DivisionByZero"
  | StepLimitExceeded -> "StepLimitExceeded"
  | NonFunctionApplication v -> "NonFunctionApplication " ^ string_of_value v
  | InvalidUnop _ -> "InvalidUnop"
  | InvalidBinop _ -> "InvalidBinop"
  | TypeError -> "TypeError"
  | LetWithoutBody -> "LetWithoutBody"
  | LetrecWithoutBody -> "LetrecWithoutBody"
  | NonIntegerCondition _ -> "NonIntegerCondition"
;;

let pp_result fmt = function
  | Ok v -> Format.fprintf fmt "Ok (%s)" (string_of_value v)
  | Error e -> Format.fprintf fmt "Error (%s)" (string_of_error e)
;;

let%expect_test "if expression true" =
  let result = interpret_optimistically "if 1 then 42 else 0" 100 in
  Format.printf "%a" pp_result (Ok result);
  [%expect {| Ok (42) |}]
;;

let%expect_test "if expression false" =
  let result = interpret_optimistically "if 0 then 42 else 100" 100 in
  Format.printf "%a" pp_result (Ok result);
  [%expect {| Ok (100) |}]
;;

let%expect_test "function creation" =
  let result =
    interpret_optimistically "let inc = fun x -> x + 1 in let x = 3 in x * inc 3" 100
  in
  Format.printf "%a" pp_result (Ok result);
  [%expect {| Ok (12) |}]
;;

let%expect_test "factorial of 5" =
  let fact_program =
    "\n\
    \    let rec fact = fun n ->\n\
    \      if 2 - n then 1 else n * fact (n - 1)\n\
    \    in\n\
    \    fact 3\n\
    \  "
  in
  let result = interpret_optimistically fact_program 1000 in
  Format.printf "%a" pp_result (Ok result);
  [%expect {| Ok (6) |}]
;;

let%expect_test "fibonacci of 6" =
  let fib_program =
    "\n\
    \    let rec fib = fun n ->\n\
    \      if 2 - n then n else fib (n - 1) + fib (n - 2)\n\
    \    in\n\
    \    fib 8\n\
    \  "
  in
  let result = interpret_optimistically fib_program Int.max_int in
  Format.printf "%a" pp_result (Ok result);
  [%expect{| Ok (21) |}]
;;

let%expect_test "print number" =
  Format.printf "%a" pp_result (Ok (interpret_optimistically "print 42" 10));
  [%expect {|
    42
    Ok (42) |}]
;;

let%expect_test "print variable" =
  Format.printf "%a" pp_result (Ok (interpret_optimistically "let x = 42 in print x" 10));
  [%expect {|
    42
    Ok (42) |}]
;;
