[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitry Arzhaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Interpeterlib.Parser
open Interpeterlib.Ast

let parse_optimistically str =
  match parser str with
  | Parsed (x, _) -> x
  | Failed _ -> failwith "parse error"
;;

let pp = pp_toplevel

let%expect_test "int constant" =
  Format.printf "%a" pp (parse_optimistically "42");
  [%expect {| 42 |}]
;;

let%expect_test "bool constant" =
  Format.printf "%a" pp (parse_optimistically "true");
  [%expect {| true |}]
;;

let%expect_test "variable" =
  Format.printf "%a" pp (parse_optimistically "x");
  [%expect {| x |}]
;;

let%expect_test "addition" =
  Format.printf "%a" pp (parse_optimistically "1 + 2");
  [%expect {| (1 + 2) |}]
;;

let%expect_test "precedence" =
  Format.printf "%a" pp (parse_optimistically "1 + 2 * 3");
  [%expect {| (1 + (2 * 3)) |}]
;;

let%expect_test "parentheses override precedence" =
  Format.printf "%a" pp (parse_optimistically "(1 + 2) * 3");
  [%expect {| ((1 + 2) * 3) |}]
;;

let%expect_test "simple application" =
  Format.printf "%a" pp (parse_optimistically "f x");
  [%expect {| (f x) |}]
;;

let%expect_test "nested application" =
  Format.printf "%a" pp (parse_optimistically "f x y");
  [%expect {| ((f x) y) |}]
;;

let%expect_test "simple function" =
  Format.printf "%a" pp (parse_optimistically "fun x -> x");
  [%expect {| fun x -> x |}]
;;

let%expect_test "multi-arg function" =
  Format.printf "%a" pp (parse_optimistically "fun x y -> x");
  [%expect {| fun x -> fun y -> x |}]
;;

let%expect_test "function application inside" =
  Format.printf "%a" pp (parse_optimistically "fun x -> x x");
  [%expect {| fun x -> (x x) |}]
;;

let%expect_test "toplevel let" =
  Format.printf "%a" pp (parse_optimistically "let x = 1");
  [%expect {| let x = 1 |}]
;;

let%expect_test "let in expression" =
  Format.printf "%a" pp (parse_optimistically "let x = 1 in x + 2");
  [%expect {| let x = 1 in (x + 2) |}]
;;

let%expect_test "if expression" =
  Format.printf "%a" pp (parse_optimistically "if true then 1 else 2");
  [%expect {| if true then 1 else 2 |}]
;;

let%expect_test "recursive function" =
  Format.printf
    "%a"
    pp
    (parse_optimistically "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1)");
  [%expect {|
    let rec fact = fun n -> if (n = 0) then 1 else (n * (fact (n - 1))) |}]
;;

let%expect_test "complex expression" =
  Format.printf "%a" pp (parse_optimistically "let f = fun x -> x + 1 in f (2 * 3)");
  [%expect {|
    let f = fun x -> (x + 1) in (f (2 * 3)) |}]
;;

let%expect_test "parse error - syntax" =
  (match parser "let = 10" with
   | Failed e -> Format.printf "%a" pp_parse_error e
   | _ -> ());
  [%expect {| syntax error |}]
;;

let%expect_test "parse error - operand" =
  (match parser "1 +" with
   | Failed e -> Format.printf "%a" pp_parse_error e
   | _ -> ());
  [%expect {| syntax error |}]
;;

let%expect_test "add" =
  Format.printf "%a" pp (parse_optimistically "1 + 2");
  [%expect {| (1 + 2) |}]
;;

let%expect_test "sub" =
  Format.printf "%a" pp (parse_optimistically "5 - 3");
  [%expect {| (5 - 3) |}]
;;

let%expect_test "mul" =
  Format.printf "%a" pp (parse_optimistically "2 * 4");
  [%expect {| (2 * 4) |}]
;;

let%expect_test "div" =
  Format.printf "%a" pp (parse_optimistically "8 / 2");
  [%expect {| (8 / 2) |}]
;;

let%expect_test "fadd" =
  Format.printf "%a" pp (parse_optimistically "1.0 +. 2.0");
  [%expect {| (1 +. 2) |}]
;;

let%expect_test "fsub" =
  Format.printf "%a" pp (parse_optimistically "5.0 -. 3.0");
  [%expect {| (5 -. 3) |}]
;;

let%expect_test "fmul" =
  Format.printf "%a" pp (parse_optimistically "2.0 *. 4.0");
  [%expect {| (2 *. 4) |}]
;;

let%expect_test "fdiv" =
  Format.printf "%a" pp (parse_optimistically "8.0 /. 2.0");
  [%expect {| (8 /. 2) |}]
;;

let%expect_test "eq" =
  Format.printf "%a" pp (parse_optimistically "1 = 2");
  [%expect {| (1 = 2) |}]
;;

let%expect_test "neq" =
  Format.printf "%a" pp (parse_optimistically "1 <> 2");
  [%expect {| (1 <> 2) |}]
;;

let%expect_test "lt" =
  Format.printf "%a" pp (parse_optimistically "1 < 2");
  [%expect {| (1 < 2) |}]
;;

let%expect_test "gt" =
  Format.printf "%a" pp (parse_optimistically "1 > 2");
  [%expect {| (1 > 2) |}]
;;

let%expect_test "leq" =
  Format.printf "%a" pp (parse_optimistically "1 <= 2");
  [%expect {| (1 <= 2) |}]
;;

let%expect_test "geq" =
  Format.printf "%a" pp (parse_optimistically "1 >= 2");
  [%expect {| (1 >= 2) |}]
;;

let%expect_test "and" =
  Format.printf "%a" pp (parse_optimistically "true && false");
  [%expect {| (true && false) |}]
;;

let%expect_test "or" =
  Format.printf "%a" pp (parse_optimistically "true || false");
  [%expect {| (true || false) |}]
;;

let%expect_test "precedence int" =
  Format.printf "%a" pp (parse_optimistically "1 + 2 * 3");
  [%expect {| (1 + (2 * 3)) |}]
;;

let%expect_test "precedence float" =
  Format.printf "%a" pp (parse_optimistically "1.0 +. 2.0 *. 3.0");
  [%expect {| (1 +. (2 *. 3)) |}]
;;

let%expect_test "comparison after arithmetic" =
  Format.printf "%a" pp (parse_optimistically "1 + 2 = 3");
  [%expect {| ((1 + 2) = 3) |}]
;;

let%expect_test "boolean after comparison" =
  Format.printf "%a" pp (parse_optimistically "1 < 2 && true");
  [%expect {| ((1 < 2) && true) |}]
;;

let%expect_test "left associativity" =
  Format.printf "%a" pp (parse_optimistically "1 - 2 - 3");
  [%expect {| ((1 - 2) - 3) |}]
;;

let%expect_test "mixed chain" =
  Format.printf "%a" pp (parse_optimistically "1 + 2 * 3 - 4");
  [%expect {| ((1 + (2 * 3)) - 4) |}]
;;
