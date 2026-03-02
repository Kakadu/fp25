[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Parser
open Printast

let run_test input =
  match parse input with
  | Ok expr -> print_endline (string_of_expr expr)
  | Error (`Parsing_error msg) -> print_endline ("Parsing error: " ^ msg)
;;

let%expect_test "parse integer" =
  run_test "42";
  [%expect {| 42 |}]
;;

let%expect_test "parse integer" =
  run_test "    42    ";
  [%expect {| 42 |}]
;;

let%expect_test "parse variable" =
  run_test "x";
  [%expect {| x |}]
;;

let%expect_test "parse simple addition" =
  run_test "1 + 2";
  [%expect {| (1 + 2) |}]
;;

let%expect_test "parse multiplication precedence" =
  run_test "1 + 2 * 3";
  [%expect {| (1 + (2 * 3)) |}]
;;

let%expect_test "parse subtraction and multiplication" =
  run_test "1 - 2 * 3";
  [%expect {| (1 - (2 * 3)) |}]
;;

let%expect_test "parse function" =
  run_test "fun x y -> x + y";
  [%expect {| (fun x y -> (x + y)) |}]
;;

let%expect_test "parse let" =
  run_test "let x = 5 in x + 1";
  [%expect {| (let x = 5 in (x + 1)) |}]
;;

let%expect_test "parse let rec" =
  run_test "let rec fact = fun n -> n in fact 5";
  [%expect {| (let rec fact = (fun n -> n) in (fact 5)) |}]
;;

let%expect_test "parse comparison" =
  run_test "x <= y";
  [%expect {| (x <= y) |}]
;;

let%expect_test "parse if expression" =
  run_test "if x then 1 else 2";
  [%expect {| (if x then 1 else 2) |}]
;;

let%expect_test "parse complex arithmetic" =
  run_test "1 + 2 * 3 - 4 / 2";
  [%expect {| ((1 + (2 * 3)) - (4 / 2)) |}]
;;

let%expect_test "parse with parentheses" =
  run_test "(1 + 2) * 3";
  [%expect {| ((1 + 2) * 3) |}]
;;

let%expect_test "parse nested parentheses" =
  run_test "((1 + 2) * (3 - 4)) / 5";
  [%expect {| (((1 + 2) * (3 - 4)) / 5) |}]
;;

let%expect_test "parse chained comparison" =
  run_test "x = y + z";
  [%expect {| (x = (y + z)) |}]
;;

let%expect_test "parse comparison with arithmetic" =
  run_test "a * b <= c + d";
  [%expect {| ((a * b) <= (c + d)) |}]
;;

let%expect_test "parse nested application" =
  run_test "f x y z";
  [%expect {| (((f x) y) z) |}]
;;

let%expect_test "parse application with arithmetic" =
  run_test "f (x + 1) (y * 2)";
  [%expect {| ((f (x + 1)) (y * 2)) |}]
;;

let%expect_test "parse lambda with complex body" =
  run_test "fun x -> x * x + 2 * x + 1";
  [%expect {| (fun x -> (((x * x) + (2 * x)) + 1)) |}]
;;

let%expect_test "parse multi-parameter lambda with arithmetic" =
  run_test "fun a b c -> a * b + c";
  [%expect {| (fun a b c -> ((a * b) + c)) |}]
;;

let%expect_test "parse let with function" =
  run_test "let add = fun x y -> x + y in add 1 2";
  [%expect {| (let add = (fun x y -> (x + y)) in ((add 1) 2)) |}]
;;

let%expect_test "parse let rec with factorial" =
  run_test "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5";
  [%expect
    {| (let rec fact = (fun n -> (if (n = 0) then 1 else (n * (fact (n - 1))))) in (fact 5)) |}]
;;

let%expect_test "parse nested let" =
  run_test "let x = 1 in let y = x + 2 in x * y";
  [%expect {| (let x = 1 in (let y = (x + 2) in (x * y))) |}]
;;

let%expect_test "parse nested if" =
  run_test "if x then if y then 1 else 2 else 3";
  [%expect {| (if x then (if y then 1 else 2) else 3) |}]
;;

let%expect_test "parse if with arithmetic" =
  run_test "if x > 0 then x + 1 else x - 1";
  [%expect {| (if (x > 0) then (x + 1) else (x - 1)) |}]
;;

let%expect_test "parse let rec fact with parameters" =
  run_test "let rec fact n = if n = 1 then 1 else n * fact (n - 1) in fact 5";
  [%expect
    {| (let rec fact = (fun n -> (if (n = 1) then 1 else (n * (fact (n - 1))))) in (fact 5)) |}]
;;

let%expect_test "parse complex mixed expression" =
  run_test "let f = fun x -> x * x in if f 5 > 20 then f (2 + 3) else 0";
  [%expect
    {| (let f = (fun x -> (x * x)) in (if ((f 5) > 20) then (f (2 + 3)) else 0)) |}]
;;

let%expect_test "parse left associativity of application" =
  run_test "f a b c";
  [%expect {| (((f a) b) c) |}]
;;

let%expect_test "parse left associativity of addition" =
  run_test "1 + 2 + 3 + 4";
  [%expect {| (((1 + 2) + 3) + 4) |}]
;;

let%expect_test "parse multiple nested parentheses" =
  run_test "(((x)))";
  [%expect {| x |}]
;;

let%expect_test "parse whitespace handling" =
  run_test "  let  x  =  5  in  x  +  1  ";
  [%expect {| (let x = 5 in (x + 1)) |}]
;;

let%expect_test "parse invalid - missing operand" =
  run_test "1 +";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse invalid - unmatched parenthesis" =
  run_test "(1 + 2";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse invalid - extra parenthesis" =
  run_test "1 + 2)";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse comparison precedence over arithmetic" =
  run_test "a + b = c * d";
  [%expect {| ((a + b) = (c * d)) |}]
;;

let%expect_test "parse fibonacci function" =
  run_test
    "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10";
  [%expect
    {| (let rec fib = (fun n -> (if (n <= 1) then n else ((fib (n - 1)) + (fib (n - 2))))) in (fib 10)) |}]
;;

let%expect_test "parse tail-recursive fibonacci" =
  run_test
    "let rec fib_helper = fun a b n -> if n = 0 then a else fib_helper b (a + b) (n - 1) \
     in let fib = fun n -> fib_helper 0 1 n in fib 10";
  [%expect
    {| (let rec fib_helper = (fun a b n -> (if (n = 0) then a else (((fib_helper b) (a + b)) (n - 1)))) in (let fib = (fun n -> (((fib_helper 0) 1) n)) in (fib 10))) |}]
;;

let%expect_test "parse tail-recursive factorial" =
  run_test
    "let rec fact_helper = fun acc n -> if n = 0 then acc else fact_helper (acc * n) (n \
     - 1) in let fact = fun n -> fact_helper 1 n in fact 5";
  [%expect
    {| (let rec fact_helper = (fun acc n -> (if (n = 0) then acc else ((fact_helper (acc * n)) (n - 1)))) in (let fact = (fun n -> ((fact_helper 1) n)) in (fact 5))) |}]
;;

let%expect_test "parse not equal operator" =
  run_test "x <> y";
  [%expect {| (x <> y) |}]
;;

let%expect_test "parse greater or equal operator" =
  run_test "x >= y";
  [%expect {| (x >= y) |}]
;;

let%expect_test "parse application before arithmetic" =
  run_test "f x + g y";
  [%expect {| ((f x) + (g y)) |}]
;;

let%expect_test "parse application with let expression" =
  run_test "f (let x = 5 in x)";
  [%expect {| (f (let x = 5 in x)) |}]
;;

let%expect_test "parse application with if expression" =
  run_test "g (if x then 1 else 2)";
  [%expect {| (g (if x then 1 else 2)) |}]
;;

let%expect_test "parse nested application with complex arguments" =
  run_test "f (let x = 1 in x) (if y then 3 else 4)";
  [%expect {| ((f (let x = 1 in x)) (if y then 3 else 4)) |}]
;;

let%expect_test "application vs multiplication priority" =
  run_test "f x * y";
  [%expect {| ((f x) * y) |}]
;;

let%expect_test "let binding if expression" =
  run_test "let x = if y then 1 else 2 in x + 3";
  [%expect {| (let x = (if y then 1 else 2) in (x + 3)) |}]
;;

let%expect_test "if with application in condition" =
  run_test "if f x then a else b";
  [%expect {| (if (f x) then a else b) |}]
;;

let%expect_test "comparison with applications" =
  run_test "f x = g y";
  [%expect {| ((f x) = (g y)) |}]
;;

let%expect_test "parse unary minus on integer" =
  run_test "-5";
  [%expect {| (-5) |}]
;;

let%expect_test "parse unary minus on variable" =
  run_test "-x";
  [%expect {| (-x) |}]
;;

let%expect_test "parse unary minus with parentheses" =
  run_test "-(x + y)";
  [%expect {| (-(x + y)) |}]
;;

let%expect_test "parse binary minus vs unary minus" =
  run_test "5 - -3";
  [%expect {| (5 - (-3)) |}]
;;

let%expect_test "parse unary minus in multiplication" =
  run_test "5 * -3";
  [%expect {| (5 * (-3)) |}]
;;

let%expect_test "parse unary minus with comparison" =
  run_test "-x < 0";
  [%expect {| ((-x) < 0) |}]
;;

let%expect_test "parse unary minus in if condition" =
  run_test "if -x then 1 else 2";
  [%expect {| (if (-x) then 1 else 2) |}]
;;

let%expect_test "parse unary minus in let binding" =
  run_test "let x = -5 in x";
  [%expect {| (let x = (-5) in x) |}]
;;

let%expect_test "parse complex expression with unary minus" =
  run_test "-x * y + -z";
  [%expect {| (((-x) * y) + (-z)) |}]
;;

let%expect_test "parse unary minus precedence over binary operators" =
  run_test "-x + y";
  [%expect {| ((-x) + y) |}]
;;

let%expect_test "parse unary minus before parentheses" =
  run_test "-(5 + 3)";
  [%expect {| (-(5 + 3)) |}]
;;

let%expect_test "parse invalid identifier starting with number" =
  run_test "123abc";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse keyword as identifier" =
  run_test "let";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse identifier with underscores" =
  run_test "_var_name_123";
  [%expect {| _var_name_123 |}]
;;

let%expect_test "parse single underscore" =
  run_test "_";
  [%expect {| _ |}]
;;

let%expect_test "parse number with underscore" =
  run_test "123_456";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse empty input" =
  run_test "";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse only spaces" =
  run_test "   ";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse only tabs" =
  run_test "\t\t\t";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse only newlines" =
  run_test "\n\n\r\n";
  [%expect {| Parsing error: syntax error |}]
;;

let%expect_test "parse only newlines" =
  run_test "\n\n\r\n5";
  [%expect {| 5 |}]
;;

let%expect_test "parse unary minus with parentheses" =
  run_test "-(-5)";
  [%expect {| (-(-5)) |}]
;;

let%expect_test "parse nested application with complex arguments with keyword var" =
  run_test "f (let let = 1 in let) (if fun then 3 else 4)";
  [%expect {| Parsing error: syntax error |}]
;;
