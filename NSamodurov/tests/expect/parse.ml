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
  parse_and_print "w (fun x -> x)";
  [%expect {| (i0 (λ . i0)) |}]
;;

let%expect_test "identity" =
  parse_and_print "(fun x -> x)";
  [%expect {| (λ . i0) |}]
;;

let%expect_test "true" =
  parse_and_print "(fun x y -> x)";
  [%expect {| (λ . (λ . i1)) |}]
;;

let%expect_test "false" =
  parse_and_print "(fun x y -> y)";
  [%expect {| (λ . (λ . i0)) |}]
;;

let%expect_test "omega comb" =
  parse_and_print "(fun x -> x x) (fun x -> x x)";
  [%expect {| ((λ . (i0 i0)) (λ . (i0 i0))) |}]
;;

let%expect_test "turing comb" =
  parse_and_print "(fun x y -> x y x) (fun x y -> x y x) ";
  [%expect {| ((λ . (λ . ((i1 i0) i1))) (λ . (λ . ((i1 i0) i1)))) |}]
;;

let%expect_test "weird function" =
  parse_and_print
    "(fun x y -> x y x) (fun x y -> x y x) (fun x y -> x y x)(fun x y -> x y x)";
  [%expect
    {| ((((λ . (λ . ((i1 i0) i1))) (λ . (λ . ((i1 i0) i1)))) (λ . (λ . ((i1 i0) i1)))) (λ . (λ . ((i1 i0) i1)))) |}]
;;

let%expect_test "plus is left associative" =
  parse_and_print "(1 + 2 + 3 + 4)";
  [%expect {| ((i-1 ((i-1 ((i-1 1) 2)) 3)) 4) |}]
;;

let%expect_test "multiplication is left associative" =
  parse_and_print "(1 * 2 * 3 * 4)";
  [%expect {| ((i-3 ((i-3 ((i-3 1) 2)) 3)) 4) |}]
;;

let%expect_test "arith prio work correctly" =
  parse_and_print "(1 + 2 * 3 + 4)";
  [%expect {| ((i-1 ((i-1 1) ((i-3 2) 3))) 4) |}]
;;

let%expect_test "let expression" =
  parse_and_print "let x = 1 in x ";
  [%expect {| let x = 1 in i0 |}]
;;

let%expect_test "let function" =
  parse_and_print "let id x = x in 1 + 2 ";
  [%expect {| let id = (λ . i0) in ((i-1 1) 2) |}]
;;

let%expect_test "let rec expression" =
  parse_and_print "let rec variable = x in y";
  [%expect {| let rec variable = i1 in i2 |}]
;;

let%expect_test "if expression 1" =
  parse_and_print "if pred then e1 else e2";
  [%expect {| if (i0) then (i1) else (i2) |}]
;;

let%expect_test "arithmetic wtih non-numbers" =
  parse_and_print "((fun x -> x) + 1)";
  [%expect {| ((i-1 (λ . i0)) 1) |}]
;;

let%expect_test "parenthesis work in arithmetic" =
  parse_and_print "(1 + (2 + 3) + 4)";
  [%expect {| ((i-1 ((i-1 1) ((i-1 2) 3))) 4) |}]
;;

let%expect_test "factorial" =
  parse_and_print "let rec fact n = if n <= 1 then n else n * fact (n-1) in fact";
  [%expect
    {| let rec fact = (λ . if (((i-7 i0) 1)) then (i0) else (((i-3 i0) (i1 ((i-2 i0) 1))))) in i0 |}]
;;

let%expect_test "rec func" =
  parse_and_print "let rec id x = if x then 1 else id true in id true";
  [%expect {| let rec id = (λ . if (i0) then (1) else ((i1 true))) in (i0 true) |}]
;;

let%expect_test "bool" =
  parse_and_print "(true)";
  [%expect {| true |}]
;;
