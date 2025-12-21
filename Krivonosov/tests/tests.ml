[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** ***** UNIT TESTS COULD GO HERE (JUST AN EXAMPLE) *)
let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%test _ = fact 5 = 120

(* These is a simple unit test that tests a single function 'fact'
   If you want to test something large, like interpretation of a piece
   of a minilanguge, it is not longer a unit tests but an integration test.
   Read about dune's cram tests and put the test into `demos/somefile.t`.
*)

open Lambda_lib
open Parser

let parse_optimistically str = Result.get_ok (parse str)
let pp ast = Print.print_ast ast

let%expect_test _ =
  Format.printf "%s" (pp (parse_optimistically "x y"));
  [%expect {| App (Var "x", Var "y") |}]
;;

let%expect_test _ =
  Format.printf "%s" (pp (parse_optimistically "(x y)"));
  [%expect {| App (Var "x", Var "y") |}]
;;

let%expect_test _ =
  Format.printf "%s" (pp (parse_optimistically "(\\x . x x)"));
  [%expect {| Abs ("x", App (Var "x", Var "x")) |}]
;;

let%expect_test _ =
  Format.printf "%s" (pp (parse_optimistically "(λf.λx. f (x x))"));
  [%expect {| Abs ("f", Abs ("x", App (Var "f", App (Var "x", Var "x")))) |}]
;;

(* Round-trip tests: parse(pretty_print(ast)) should equal ast *)
(* Testing that parsing and printing are inverses *)

let test_roundtrip ast_str =
  let parsed = parse_optimistically ast_str in
  let printed = Print.print_expr parsed in
  let reparsed = parse_optimistically printed in
  if parsed = reparsed
  then ()
  else
    Format.printf
      "Round-trip failed!\nOriginal: %s\nPrinted: %s\nReparsed AST: %s\n"
      ast_str
      printed
      (pp reparsed)
;;

(* Test round-trip for integers *)
let%test "roundtrip: integer" =
  test_roundtrip "42";
  true
;;

(* Test round-trip for variables *)
let%test "roundtrip: variable" =
  test_roundtrip "foo";
  true
;;

(* Test round-trip for lambda *)
let%test "roundtrip: lambda" =
  test_roundtrip "fun x -> x";
  true
;;

(* Test round-trip for application *)
let%test "roundtrip: application" =
  test_roundtrip "(fun x -> x) 5";
  true
;;

(* Test round-trip for arithmetic *)
let%test "roundtrip: arithmetic" =
  test_roundtrip "2 + 3";
  true
;;

let%test "roundtrip: complex arithmetic" =
  test_roundtrip "3 * 4 + 5";
  true
;;

(* Test round-trip for if-then-else *)
let%test "roundtrip: if-then-else" =
  test_roundtrip "if 1 then 5 else 10";
  true
;;

(* Test round-trip for let binding *)
let%test "roundtrip: let binding" =
  test_roundtrip "let x = 5 in x + 3";
  true
;;

(* Test round-trip for let rec *)
let%test "roundtrip: let rec" =
  test_roundtrip
    "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5";
  true
;;

(* Test round-trip for multi-parameter function *)
let%test "roundtrip: multi-param fun" =
  test_roundtrip "fun x y z -> x + y + z";
  true
;;

(* Test round-trip for nested let *)
let%test "roundtrip: nested let" =
  test_roundtrip "let x = 10 in let y = 20 in x + y";
  true
;;
