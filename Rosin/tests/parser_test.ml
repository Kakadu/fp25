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

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "let rec x = 2 in x + 2");
  [%expect{| Letrec((x, 2) in (Plus(x, 2))) |}]
;;