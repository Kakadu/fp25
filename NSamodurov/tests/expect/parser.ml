[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Parser
open Utils

let parse_and_print str =
  match parse str with
  | Result.Ok v ->
    let b = to_brujin v in
    Format.printf "%a\n" Pprintast.pp_brujin b
  | Result.Error e -> Format.printf "Error: %a" pp_error e
;;

let%expect_test "unbound" =
  parse_and_print "x y z";
  [%expect {| ((i13 i14) i15) |}]
;;

let%expect_test "bound and unbound" =
  parse_and_print "w (fun x -> x)";
  [%expect {| (i13 (λ . i13)) |}]
;;

let%expect_test "identity" =
  parse_and_print "(fun x -> x)";
  [%expect {| (λ . i13) |}]
;;

let%expect_test "true" =
  parse_and_print "(fun x y -> x)";
  [%expect {| (λ . (λ . i14)) |}]
;;

let%expect_test "false" =
  parse_and_print "(fun x y -> y)";
  [%expect {| (λ . (λ . i13)) |}]
;;

let%expect_test "omega comb" =
  parse_and_print "(fun x -> x x) (fun x -> x x)";
  [%expect {| ((λ . (i13 i13)) (λ . (i13 i13))) |}]
;;

let%expect_test "turing comb" =
  parse_and_print "(fun x y -> x y x) (fun x y -> x y x) ";
  [%expect {| ((λ . (λ . ((i14 i13) i14))) (λ . (λ . ((i14 i13) i14)))) |}]
;;

let%expect_test "weird function" =
  parse_and_print
    "(fun x y -> x y x) (fun x y -> x y x) (fun x y -> x y x)(fun x y -> x y x)";
  [%expect
    {| ((((λ . (λ . ((i14 i13) i14))) (λ . (λ . ((i14 i13) i14)))) (λ . (λ . ((i14 i13) i14)))) (λ . (λ . ((i14 i13) i14)))) |}]
;;

let%expect_test "plus is left associative" =
  parse_and_print "(1 + 2 + 3 + 4)";
  [%expect {| (((1 + 2) + 3) + 4) |}]
;;

let%expect_test "multiplication is left associative" =
  parse_and_print "(1 * 2 * 3 * 4)";
  [%expect {| (((1 * 2) * 3) * 4) |}]
;;

let%expect_test "arith prio work correctly" =
  parse_and_print "(1 + 2 * 3 + 4)";
  [%expect {| ((1 + (2 * 3)) + 4) |}]
;;

let%expect_test "let expression" =
  parse_and_print "let x = 1 in x ";
  [%expect {| let i13 = 1 in i13 |}]
;;

let%expect_test "let function" =
  parse_and_print "let id x = x in 1 + 2 ";
  [%expect {| let i13 = (λ . i13) in (1 + 2) |}]
;;

let%expect_test "let rec expression" =
  parse_and_print "let rec variable = x in y";
  [%expect {| let rec i13 = i14 in i15 |}]
;;

let%expect_test "if expression 1" =
  parse_and_print "if pred then e1 else e2";
  [%expect {| if (i13) then (i14) else (i15) |}]
;;

let%expect_test "arithmetic wtih non-numbers" =
  parse_and_print "((fun x -> x) + 1)";
  [%expect {| ((λ . i13) + 1) |}]
;;

let%expect_test "parenthesis work in arithmetic" =
  parse_and_print "(1 + (2 + 3) + 4)";
  [%expect {| ((1 + (2 + 3)) + 4) |}]
;;

let%expect_test "factorial" =
  parse_and_print "let rec id x = if true then x * id (x-1) else 0 in id 1";
  [%expect
    {| let rec i13 = (λ . if (((i4 i13) 0)) then ((i13 * (i14 (i13 - 1)))) else (0)) in (i13 1) |}]
;;

let%expect_test "compare" =
  parse_and_print "(true)";
  [%expect {| true |}]
;;
