[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib.Print
open Lambda_lib.Parser

let%expect_test "parse number" =
  print_string (print_ast (parse "42" |> Result.get_ok));
  [%expect {| 42 |}]
;;

let%expect_test "parse variable" =
  print_string (print_ast (parse "x" |> Result.get_ok));
  [%expect {| x |}]
;;

let%expect_test "parse addition" =
  print_string (print_ast (parse "2 + 3" |> Result.get_ok));
  [%expect {| (2 + 3) |}]
;;

let%expect_test "parse multiplication" =
  print_string (print_ast (parse "2 * 3" |> Result.get_ok));
  [%expect {| (2 * 3) |}]
;;

let%expect_test "parse parentheses" =
  print_string (print_ast (parse "(2 + 3)" |> Result.get_ok));
  [%expect {| (2 + 3) |}]
;;

let%expect_test "parse comparison" =
  print_string (print_ast (parse "2 > 3" |> Result.get_ok));
  [%expect {| (2 > 3) |}]
;;

let%expect_test "parse conditional" =
  print_string (print_ast (parse "if 1 > 0 then 2 else 3" |> Result.get_ok));
  [%expect {| (if (1 > 0) then 2 else 3) |}]
;;

let%expect_test "parse let binding" =
  print_string (print_ast (parse "let x = 5 in x" |> Result.get_ok));
  [%expect {| (let x = 5 in x) |}]
;;

let%expect_test "parse application" =
  print_string (print_ast (parse "f x" |> Result.get_ok));
  [%expect {| f x |}]
;;

let%expect_test "parse anonymous function" =
  print_string (print_ast (parse "fun x -> x + 1" |> Result.get_ok));
  [%expect {| (fun x -> (x + 1)) |}]
;;

let%expect_test "parse negative number" =
  print_string (print_ast (parse "-5" |> Result.get_ok));
  [%expect {| -5 |}]
;;

let%expect_test "parse complex expression" =
  print_string (print_ast (parse "2 + 3 * 4" |> Result.get_ok));
  [%expect {| (2 + (3 * 4)) |}]
;;

let%expect_test "parse function with multiple args" =
  print_string (print_ast (parse "let add x y = x + y in add 1 2" |> Result.get_ok));
  [%expect {| (let add = (fun x -> (fun y -> (x + y))) in add 1 2) |}]
;;

let%expect_test "parse nested application" =
  print_string (print_ast (parse "f (g x)" |> Result.get_ok));
  [%expect {| f g x |}]
;;

let%expect_test "factorial with rec" =
  print_string
    (print_ast
       (parse "let rec fac n= if n = 1 then 1 else (fac (n-1)) * n" |> Result.get_ok));
  [%expect {| (let rec fac = (fun n -> (if (n = 1) then 1 else (fac (n - 1) * n)))) |}]
;;

let%expect_test "some skope values" =
  let expresion =
    "let result =\n\
    \  let sterter x = if x + (8 * 9) > 4 then x * 2 else x * 5 in\n\
    \    let inner pp = sterter pp + 8 in\n\
    \  inner 8"
  in
  print_string (print_ast (parse expresion |> Result.get_ok));
  [%expect
    {| (let result = (let sterter = (fun x -> (if ((x + (8 * 9)) > 4) then (x * 2) else (x * 5))) in (let inner = (fun pp -> (sterter pp + 8)) in inner 8))) |}]
;;

let%expect_test "function annotation via fun" =
  print_string (print_ast (parse "let function = fun a -> a * a" |> Result.get_ok));
  [%expect {| (let function = (fun a -> (a * a))) |}]
;;

let%expect_test "application with lambda" =
  print_string (print_ast (parse "let tmp = (fun a -> a * a) 5" |> Result.get_ok));
  [%expect {| (let tmp = (fun a -> (a * a)) 5) |}]
;;

let%expect_test "unbounded lambda" =
  print_string (print_ast (parse "(fun a -> a * a)" |> Result.get_ok));
  [%expect {| (fun a -> (a * a)) |}]
;;

let%expect_test "simple application" =
  prerr_string (print_ast (parse "let k = g 4 (f 5)" |> Result.get_ok));
  [%expect {| (let k = g 4 f 5) |}]
;;

let%expect_test "nested let with application" =
  print_string
    (print_ast (parse "let f = fun x -> x + 1 in let y = f 5 in y * 2" |> Result.get_ok));
  [%expect {| (let f = (fun x -> (x + 1)) in (let y = f 5 in (y * 2))) |}]
;;

let%expect_test "anonymous function in application" =
  print_string (print_ast (parse "(fun x -> x * 2) 10" |> Result.get_ok));
  [%expect {| (fun x -> (x * 2)) 10 |}]
;;

let%expect_test "multiple applications with arithmetic" =
  print_string (print_ast (parse "f (g x) + h (k y) * 2" |> Result.get_ok));
  [%expect {| (f g x + (h k y * 2)) |}]
;;

let%expect_test "conditional with applications" =
  print_string (print_ast (parse "if f x > 0 then g y else h z" |> Result.get_ok));
  [%expect {| (if (f x > 0) then g y else h z) |}]
;;

let%expect_test "nested anonymous functions" =
  print_string (print_ast (parse "fun x -> fun y -> x + y" |> Result.get_ok));
  [%expect {| (fun x -> (fun y -> (x + y))) |}]
;;

let%expect_test "let with multiple arguments and application" =
  print_string
    (print_ast (parse "let add x y = x + y in add (f 1) (g 2)" |> Result.get_ok));
  [%expect {| (let add = (fun x -> (fun y -> (x + y))) in add f 1 g 2) |}]
;;

let%expect_test "application chain with comparison" =
  print_string (print_ast (parse "f a b > g c d + 1" |> Result.get_ok));
  [%expect {| (f a b > (g c d + 1)) |}]
;;

let%expect_test "nested lets with applications" =
  print_string
    (print_ast
       (parse "let x = 5 in let f = fun y -> x + y in let z = f 10 in z * 2"
        |> Result.get_ok));
  [%expect {| (let x = 5 in (let f = (fun y -> (x + y)) in (let z = f 10 in (z * 2)))) |}]
;;

let%expect_test "application with complex expressions" =
  print_string
    (print_ast (parse "f (x + 1) (y * 2) (if z > 0 then a else b)" |> Result.get_ok));
  [%expect {| f (x + 1) (y * 2) (if (z > 0) then a else b) |}]
;;

let%expect_test "recursive function with application" =
  print_string
    (print_ast
       (parse "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5"
        |> Result.get_ok));
  [%expect
    {| (let rec fact = (fun n -> (if (n = 0) then 1 else (n * fact (n - 1)))) in fact 5) |}]
;;

let%expect_test "right application" =
  print_string (print_ast (parse "g (f (q 5))" |> Result.get_ok));
  [%expect {| g f q 5 |}]
;;

let%expect_test "nested if-then-else" =
  print_string
    (print_ast
       (parse "if 4>8 then if 7>9 then 7 else g 7 else g (k 4) 7" |> Result.get_ok));
  [%expect {| (if (4 > 8) then (if (7 > 9) then 7 else g 7) else g k 4 7) |}]
;;

let%expect_test "test else if" =
  print_string
    (print_ast (parse "if 4>8 then g 7 4 h else if 7>9 then 7 else g 7" |> Result.get_ok));
  [%expect {| (if (4 > 8) then g 7 4 h else (if (7 > 9) then 7 else g 7)) |}]
;;
