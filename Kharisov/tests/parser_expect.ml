[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Interpret_lib

let show s =
  match Parser.parse s with
  | Ok e -> print_endline (Pprint.to_string e)
  | Error (`Parse_error msg) -> print_endline ("Parse error: " ^ msg)
;;

let%expect_test "integer" =
  show "42";
  [%expect {| 42 |}]
;;

let%expect_test "integer with whitespace" =
  show "   0   ";
  [%expect {| 0 |}]
;;

let%expect_test "variable" =
  show "x";
  [%expect {| x |}]
;;

let%expect_test "true/false sugar" =
  show "true";
  [%expect {| 1 |}];
  show "false";
  [%expect {| 0 |}]
;;

let%expect_test "addition" =
  show "1 + 2";
  [%expect {| (1 + 2) |}]
;;

let%expect_test "mul before add" =
  show "1 + 2 * 3";
  [%expect {| (1 + (2 * 3)) |}]
;;

let%expect_test "parentheses override precedence" =
  show "(1 + 2) * 3";
  [%expect {| ((1 + 2) * 3) |}]
;;

let%expect_test "left-associative subtraction" =
  show "10 - 3 - 2";
  [%expect {| ((10 - 3) - 2) |}]
;;

let%expect_test "left-associative division" =
  show "8 / 2 / 2";
  [%expect {| ((8 / 2) / 2) |}]
;;

let%expect_test "complex arithmetic" =
  show "1 + 2 * 3 - 4 / 2";
  [%expect {| ((1 + (2 * 3)) - (4 / 2)) |}]
;;

let%expect_test "unary minus" =
  show "-5 + 3";
  [%expect {| ((-5) + 3) |}]
;;

let%expect_test "eq" =
  show "x = 5";
  [%expect {| (x = 5) |}]
;;

let%expect_test "neq" =
  show "x <> 0";
  [%expect {| (x <> 0) |}]
;;

let%expect_test "lt le gt ge" =
  show "a < b";
  [%expect {| (a < b) |}];
  show "a <= b";
  [%expect {| (a <= b) |}];
  show "a > b";
  [%expect {| (a > b) |}];
  show "a >= b";
  [%expect {| (a >= b) |}]
;;

let%expect_test "comparison with arithmetic" =
  show "a * b <= c + d";
  [%expect {| ((a * b) <= (c + d)) |}]
;;

let%expect_test "lambda" =
  show "fun x -> x + 1";
  [%expect {| (fun x -> (x + 1)) |}]
;;

let%expect_test "multi-arg lambda sugar" =
  show "fun x y z -> x";
  [%expect {| (fun x -> (fun y -> (fun z -> x))) |}]
;;

let%expect_test "application" =
  show "f x";
  [%expect {| (f x) |}]
;;

let%expect_test "nested application (left-assoc)" =
  show "f x y z";
  [%expect {| (((f x) y) z) |}]
;;

let%expect_test "application of complex arg" =
  show "f (x + 1)";
  [%expect {| (f (x + 1)) |}]
;;

let%expect_test "let" =
  show "let x = 5 in x + 1";
  [%expect {| (let x = 5 in (x + 1)) |}]
;;

let%expect_test "let with function sugar" =
  show "let f x y = x + y in f 1 2";
  [%expect {| (let f = (fun x -> (fun y -> (x + y))) in ((f 1) 2)) |}]
;;

let%expect_test "let rec" =
  show "let rec f n = n * f (n - 1) in f 5";
  [%expect {| (let rec f = (fun n -> (n * (f (n - 1)))) in (f 5)) |}]
;;

let%expect_test "nested let" =
  show "let x = 1 in let y = 2 in x + y";
  [%expect {| (let x = 1 in (let y = 2 in (x + y))) |}]
;;

let%expect_test "if-then-else" =
  show "if x = 0 then 1 else 2";
  [%expect {| (if (x = 0) then 1 else 2) |}]
;;

let%expect_test "nested if" =
  show "if x then if y then 1 else 2 else 3";
  [%expect {| (if x then (if y then 1 else 2) else 3) |}]
;;

let%expect_test "keyword-like identifiers" =
  show "let funx = 1 in funx";
  [%expect {| (let funx = 1 in funx) |}];
  show "let letx = 1 in letx";
  [%expect {| (let letx = 1 in letx) |}];
  show "let ifx = 1 in ifx";
  [%expect {| (let ifx = 1 in ifx) |}]
;;

let%expect_test "error: missing rhs in let" =
  show "let x = in 5";
  [%expect {| Parse error: : no more choices |}]
;;

let%expect_test "error: empty input" =
  show "";
  [%expect {| Parse error: : no more choices |}]
;;

let%expect_test "juxtaposition parses as application" =
  show "1 2 3";
  [%expect {| ((1 2) 3) |}]
;;

let%expect_test "digit-letter suffix rejected (e.g., 3x)" =
  show "3x";
  [%expect {| Parse error: : no more choices |}]
;;

let%expect_test "integer overflow" =
  show "99999999999999999999";
  [%expect {| Parse error: : no more choices |}]
;;

let%expect_test "parenthesized identifier in fun" =
  show "fun (x) -> x";
  [%expect {| (fun x -> x) |}]
;;

let%expect_test "parenthesized identifier in let" =
  show "let (f) x = x in f 5";
  [%expect {| (let f = (fun x -> x) in (f 5)) |}]
;;
