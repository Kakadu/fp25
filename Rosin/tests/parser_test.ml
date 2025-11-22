[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib
open Parser

let parse_optimistically str = Result.get_ok (parse str)
let parse_pessimistically str =
  match parse str with
  | Error _ -> true
  | _ -> false
let pp = Pprintast.pp

let%expect_test _ =
  assert(parse_pessimistically "")
;;

let%expect_test "parse number" =
  Format.printf "%a" pp (parse_optimistically "42");
  [%expect{| 42 |}]
;;
let%expect_test "parse negative number" =
  Format.printf "%a" pp (parse_optimistically "-42");
  [%expect{| -42 |}]
;;

let%expect_test "parse variable" =
  Format.printf "%a" pp (parse_optimistically "x");
  [%expect{| x |}]
;;

let%expect_test "parse number sum" =
  Format.printf "%a" pp (parse_optimistically "2 + 2");
  [%expect{| (Plus(2, 2)) |}]
;;

let%expect_test "parse number and variable sum" =
  Format.printf "%a" pp (parse_optimistically "x + 2");
  [%expect{| (Plus(x, 2)) |}]
;;

let%expect_test "parse binary operation chain with multiplication" =
  Format.printf "%a" pp (parse_optimistically "y + 2 * x");
  [%expect{| (Plus(y, (Mult(2, x)))) |}]
;;

let%expect_test "parse binary operation chain with division" =
  Format.printf "%a" pp (parse_optimistically "y + 2 / x");
  [%expect{| (Plus(y, (Div(2, x)))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "let rec x = 2 in x + 2");
  [%expect{| Letrec((x, 2) in (Plus(x, 2))) |}]
;;