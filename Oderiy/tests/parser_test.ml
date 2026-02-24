[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "*/"]

open Mml.Parser
open Mml.Print

let check_parse input expected =
  match parse input with
  | Ok ast ->
    let result = to_string ast in
    if String.equal result expected
    then print_string result
    else Printf.printf "Expected: %s\nGot: %s" expected result
  | Error (`Parsing_error msg) -> Printf.printf "Parse error: %s" msg
;;

let%expect_test "parse positive integer" =
  check_parse "123" "123";
  [%expect {| 123 |}]
;;

let%expect_test "parse zero" =
  check_parse "0" "0";
  [%expect {| 0 |}]
;;

let%expect_test "parse negative integer" =
  check_parse "-42" "(-42)";
  [%expect {| (-42) |}]
;;

(* Test identifiers *)
let%expect_test "parse identifier" =
  check_parse "foo" "foo";
  [%expect {| foo |}]
;;

let%expect_test "parse identifier with underscore and digits" =
  check_parse "_bar123" "_bar123";
  [%expect {| _bar123 |}]
;;

(* Test arithmetic expressions *)
let%expect_test "parse addition" =
  check_parse "3 + 5" "(3 + 5)";
  [%expect {| (3 + 5) |}]
;;

let%expect_test "parse subtraction" =
  check_parse "12 - 7" "(12 - 7)";
  [%expect {| (12 - 7) |}]
;;

let%expect_test "parse multiplication" =
  check_parse "6 * 7" "(6 * 7)";
  [%expect {| (6 * 7) |}]
;;

let%expect_test "parse division" =
  check_parse "15 / 3" "(15 / 3)";
  [%expect {| (15 / 3) |}]
;;

let%expect_test "parse expression with operator precedence" =
  check_parse "1 + 2 * 3" "(1 + (2 * 3))";
  [%expect {| (1 + (2 * 3)) |}]
;;

let%expect_test "parse parenthesized expression" =
  check_parse "(1 + 2) * 3" "((1 + 2) * 3)";
  [%expect {| ((1 + 2) * 3) |}]
;;

(* Test comparison operators *)
let%expect_test "parse less than" =
  check_parse "x < y" "(x < y)";
  [%expect {| (x < y) |}]
;;

let%expect_test "parse less than or equal" =
  check_parse "a <= b" "(a <= b)";
  [%expect {| (a <= b) |}]
;;

let%expect_test "parse equality" =
  check_parse "m = n" "(m = n)";
  [%expect {| (m = n) |}]
;;

let%expect_test "parse greater than or equal" =
  check_parse "p >= q" "(p >= q)";
  [%expect {| (p >= q) |}]
;;

let%expect_test "parse greater than" =
  check_parse "s > t" "(s > t)";
  [%expect {| (s > t) |}]
;;

(* Test lambda abstractions *)
let%expect_test "parse identity lambda" =
  check_parse "fun x -> x" "(fun x -> x)";
  [%expect {| (fun x -> x) |}]
;;

let%expect_test "parse lambda with expression body" =
  check_parse "fun a -> a + 1" "(fun a -> (a + 1))";
  [%expect {| (fun a -> (a + 1)) |}]
;;

let%expect_test "parse multi-argument lambda" =
  check_parse "fun x y -> x + y" "(fun x -> (fun y -> (x + y)))";
  [%expect {| (fun x -> (fun y -> (x + y))) |}]
;;

let%expect_test "parse nested lambda" =
  check_parse "fun x -> fun y -> x * y" "(fun x -> (fun y -> (x * y)))";
  [%expect {| (fun x -> (fun y -> (x * y))) |}]
;;

(* Test function applications *)
let%expect_test "parse simple application" =
  check_parse "f x" "(f x)";
  [%expect {| (f x) |}]
;;

let%expect_test "parse multiple application" =
  check_parse "g a b" "((g a) b)";
  [%expect {| ((g a) b) |}]
;;

let%expect_test "parse lambda application" =
  check_parse "(fun x -> x + 1) 10" "((fun x -> (x + 1)) 10)";
  [%expect {| ((fun x -> (x + 1)) 10) |}]
;;

(* Test let expressions *)
let%expect_test "parse simple let binding" =
  check_parse "let x = 10 in x" "(let x = 10 in x)";
  [%expect {| (let x = 10 in x) |}]
;;

let%expect_test "parse let binding with expression" =
  check_parse "let y = 5 in y * 2" "(let y = 5 in (y * 2))";
  [%expect {| (let y = 5 in (y * 2)) |}]
;;

let%expect_test "parse nested let bindings" =
  check_parse "let a = 1 in let b = 2 in a + b" "(let a = 1 in (let b = 2 in (a + b)))";
  [%expect {| (let a = 1 in (let b = 2 in (a + b))) |}]
;;

let%expect_test "parse let with function binding" =
  check_parse "let f x = x * 2 in f 5" "(let f = (fun x -> (x * 2)) in (f 5))";
  [%expect {| (let f = (fun x -> (x * 2)) in (f 5)) |}]
;;

(* Test recursive let *)
let%expect_test "parse recursive factorial" =
  check_parse
    "let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5"
    "(let rec fact = (fun n -> (if (n <= 1) then 1 else (n * (fact (n - 1))))) in (fact \
     5))";
  [%expect
    {| (let rec fact = (fun n -> (if (n <= 1) then 1 else (n * (fact (n - 1))))) in (fact 5)) |}]
;;

let%expect_test "parse recursive fibonacci" =
  check_parse
    "let rec fib n = if n <= 1 then n else fib (n-1) + fib (n-2) in fib 8"
    "(let rec fib = (fun n -> (if (n <= 1) then n else ((fib (n - 1)) + (fib (n - 2))))) \
     in (fib 8))";
  [%expect
    {| (let rec fib = (fun n -> (if (n <= 1) then n else ((fib (n - 1)) + (fib (n - 2))))) in (fib 8)) |}]
;;

(* Test conditionals *)
let%expect_test "parse simple conditional" =
  check_parse "if x then 1 else 0" "(if x then 1 else 0)";
  [%expect {| (if x then 1 else 0) |}]
;;

let%expect_test "parse conditional with comparison" =
  check_parse "if a > b then a else b" "(if (a > b) then a else b)";
  [%expect {| (if (a > b) then a else b) |}]
;;

let%expect_test "parse nested conditionals" =
  check_parse
    "if x > 0 then if x > 10 then 2 else 1 else 0"
    "(if (x > 0) then (if (x > 10) then 2 else 1) else 0)";
  [%expect {| (if (x > 0) then (if (x > 10) then 2 else 1) else 0) |}]
;;

(* Test combined expressions *)
let%expect_test "parse complex arithmetic expression" =
  check_parse "1+2*3-4/2" "((1 + (2 * 3)) - (4 / 2))";
  [%expect {| ((1 + (2 * 3)) - (4 / 2)) |}]
;;

let%expect_test "parse multiple let bindings with arithmetic" =
  check_parse
    "let x = 3 in let y = 4 in let z = 5 in x * y + z"
    "(let x = 3 in (let y = 4 in (let z = 5 in ((x * y) + z))))";
  [%expect {| (let x = 3 in (let y = 4 in (let z = 5 in ((x * y) + z)))) |}]
;;

(* Test that invalid syntax is rejected *)
let%expect_test "reject invalid number-identifier mix" =
  check_parse "123abc" "should fail";
  [%expect {| Parse error: : no more choices |}]
;;
