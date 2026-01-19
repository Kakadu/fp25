(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Filichkin_lib.Parser

let assert_parse_error input =
  match parser input with
  | Ok _ -> print_string "Unexpected success"
  | Error (`parse_error _) -> print_string "Parse error"
;;

let%expect_test "parse error: incomplete let" =
  assert_parse_error "let x = in x";
  [%expect {| Parse error |}]
;;

let%expect_test "parse error: incomplete type" =
  assert_parse_error "type t =";
  [%expect {| Parse error |}]
;;

let%expect_test "parse error: match without cases" =
  assert_parse_error "match x with";
  [%expect {| Parse error |}]
;;
