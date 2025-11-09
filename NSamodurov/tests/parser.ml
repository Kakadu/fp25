[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Parser

let parse_and_print str =
  match parse str with
  | Result.Ok v ->
    let b = to_brujin v in
    Format.printf "%a\n" Pprintast.pp_brujin b
  | Result.Error e -> Format.printf "Error: %a" Parser.pp_error e
;;

let%expect_test "unbound" =
  parse_and_print "x y z";
  [%expect {| ((i0 i1) i2) |}]
;;

let%expect_test "bound and unbound" =
  parse_and_print "w fun x -> x";
  [%expect {| (i0 (λ . i1)) |}]
;;

let%expect_test "true" =
  parse_and_print "fun x y -> x";
  [%expect {| (λ . (λ . i0)) |}]
;;

let%expect_test "false" =
  parse_and_print "fun x y -> y";
  [%expect {| (λ . (λ . i1)) |}]
;;

let%expect_test "omega comb" =
  parse_and_print "(fun x -> x x ) (fun x -> x x)";
  [%expect {| ((λ . (i0 i0)) (λ . (i0 i0))) |}]
;;

let%expect_test "turing comb" =
  parse_and_print "(fun x y -> x y x) (fun x y -> x y x) ";
  [%expect {| ((λ . (λ . ((i0 i1) i0))) (λ . (λ . ((i1 i0) i1)))) |}]
;;

let%expect_test "plus is left associative" =
  parse_and_print "1 + 2 + 3 + 4";
  [%expect {| (((1 + 2) + 3) + 4) |}]
;;

let%expect_test "multiplication is left associative" =
  parse_and_print "1 * 2 * 3 * 4";
  [%expect {| (((1 * 2) * 3) * 4) |}]
;;

let%expect_test "arith prio work correctly" =
  parse_and_print "1 + 2 * 3 + 4";
  [%expect {| ((1 + (2 * 3)) + 4) |}]
;;

let%expect_test "let expression" =
  parse_and_print "let variable = x in y";
  [%expect {| Error: : end_of_input |}]
;;

let%expect_test "let rec expression" =
  parse_and_print "let rec variable = x in y";
  [%expect {| Error: : end_of_input |}]
;;

let%expect_test "arithmetic wtih non-numbers" =
  parse_and_print "(fun x -> x) + 1";
  [%expect {| Error: : end_of_input |}]
;;

let%expect_test "parenthesis work in arithmetic" =
  parse_and_print "1 + (2 + 3) + 4";
  [%expect {| Error: : end_of_input |}]
;;
