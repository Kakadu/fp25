[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib.Print
open Lambda_lib.Parser

let%expect_test "parse number" =
  print_string (print_ast (parse "42" |> Result.get_ok));
  [%expect {| CInt(42) |}]
;;

let%expect_test "parse variable" =
  print_string (print_ast (parse "x" |> Result.get_ok));
  [%expect {| Var(x) |}]
;;

let%expect_test "parse addition" =
  print_string (print_ast (parse "2 + 3" |> Result.get_ok));
  [%expect {| (CInt(2) + CInt(3)) |}]
;;

let%expect_test "parse multiplication" =
  print_string (print_ast (parse "2 * 3" |> Result.get_ok));
  [%expect {| (CInt(2) * CInt(3)) |}]
;;

let%expect_test "parse parentheses" =
  print_string (print_ast (parse "(2 + 3)" |> Result.get_ok));
  [%expect {| (CInt(2) + CInt(3)) |}]
;;

let%expect_test "parse comparison" =
  print_string (print_ast (parse "2 > 3" |> Result.get_ok));
  [%expect {| (CInt(2) > CInt(3)) |}]
;;

let%expect_test "parse conditional" =
  print_string (print_ast (parse "if 1 > 0 then 2 else 3" |> Result.get_ok));
  [%expect {| if ((CInt(1) > CInt(0))) then (CInt(2)) else (CInt(3)) |}]
;;

let%expect_test "parse let binding" =
  print_string (print_ast (parse "let x = 5 in x" |> Result.get_ok));
  [%expect {| (let x = CInt(5) in Var(x)) |}]
;;

let%expect_test "parse application" =
  print_string (print_ast (parse "f x" |> Result.get_ok));
  [%expect {| App (Var(f), Var(x)) |}]
;;

let%expect_test "parse anonymous function" =
  print_string (print_ast (parse "fun x -> x + 1" |> Result.get_ok));
  [%expect {| Fun (Var(x), (Var(x) + CInt(1))) |}]
;;

let%expect_test "parse negative number" =
  print_string (print_ast (parse "-5" |> Result.get_ok));
  [%expect {| CInt(-5) |}]
;;

let%expect_test "parse complex expression" =
  print_string (print_ast (parse "2 + 3 * 4" |> Result.get_ok));
  [%expect {| (CInt(2) + (CInt(3) * CInt(4))) |}]
;;

let%expect_test "parse function with multiple args" =
  print_string (print_ast (parse "let add x y = x + y in add 1 2" |> Result.get_ok));
  [%expect
    {| (let add = Fun (Var(x), Fun (Var(y), (Var(x) + Var(y)))) in App (App (Var(add), CInt(1)), CInt(2))) |}]
;;

let%expect_test "parse nested application" =
  print_string (print_ast (parse "f (g x)" |> Result.get_ok));
  [%expect {| App (Var(f), App (Var(g), Var(x))) |}]
;;

let%expect_test "factorial with rec" =
  print_string
    (print_ast
       (parse "let rec fac n= if n = 1 then 1 else (fac (n-1)) * n" |> Result.get_ok));
  [%expect
    {| (let rec fac = Fun (Var(n), if ((Var(n) = CInt(1))) then (CInt(1)) else ((App (Var(fac), (Var(n) - CInt(1))) * Var(n))))) |}]
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
    {| (let result = (let sterter = Fun (Var(x), if (((Var(x) + (CInt(8) * CInt(9))) > CInt(4))) then ((Var(x) * CInt(2))) else ((Var(x) * CInt(5)))) in (let inner = Fun (Var(pp), (App (Var(sterter), Var(pp)) + CInt(8))) in App (Var(inner), CInt(8))))) |}]
;;

let%expect_test "function annotation via fun" =
  print_string (print_ast (parse "let function = fun a -> a * a" |> Result.get_ok));
  [%expect {| (let function = Fun (Var(a), (Var(a) * Var(a)))) |}]
;;

let%expect_test "application with lambda" =
  print_string (print_ast (parse "let tmp = (fun a -> a * a) 5" |> Result.get_ok));
  [%expect {| (let tmp = App (Fun (Var(a), (Var(a) * Var(a))), CInt(5))) |}]
;;

let%expect_test "unbounded lambda" =
  print_string (print_ast (parse "(fun a -> a * a)" |> Result.get_ok));
  [%expect {| Fun (Var(a), (Var(a) * Var(a))) |}]
;;

let%expect_test "simple application" =
  prerr_string (print_ast (parse "let _ = g 4 (f 5)" |> Result.get_ok));
  [%expect {| (let _ = App (App (Var(g), CInt(4)), App (Var(f), CInt(5)))) |}]
;;

let%expect_test "nested let with application" =
  print_string
    (print_ast (parse "let f = fun x -> x + 1 in let y = f 5 in y * 2" |> Result.get_ok));
  [%expect
    {| (let f = Fun (Var(x), (Var(x) + CInt(1))) in (let y = App (Var(f), CInt(5)) in (Var(y) * CInt(2)))) |}]
;;

let%expect_test "anonymous function in application" =
  print_string (print_ast (parse "(fun x -> x * 2) 10" |> Result.get_ok));
  [%expect {| App (Fun (Var(x), (Var(x) * CInt(2))), CInt(10)) |}]
;;

let%expect_test "multiple applications with arithmetic" =
  print_string (print_ast (parse "f (g x) + h (k y) * 2" |> Result.get_ok));
  [%expect
    {| (App (Var(f), App (Var(g), Var(x))) + (App (Var(h), App (Var(k), Var(y))) * CInt(2))) |}]
;;

let%expect_test "conditional with applications" =
  print_string (print_ast (parse "if f x > 0 then g y else h z" |> Result.get_ok));
  [%expect
    {| if ((App (Var(f), Var(x)) > CInt(0))) then (App (Var(g), Var(y))) else (App (Var(h), Var(z))) |}]
;;

let%expect_test "nested anonymous functions" =
  print_string (print_ast (parse "fun x -> fun y -> x + y" |> Result.get_ok));
  [%expect {| Fun (Var(x), Fun (Var(y), (Var(x) + Var(y)))) |}]
;;

let%expect_test "let with multiple arguments and application" =
  print_string
    (print_ast (parse "let add x y = x + y in add (f 1) (g 2)" |> Result.get_ok));
  [%expect
    {| (let add = Fun (Var(x), Fun (Var(y), (Var(x) + Var(y)))) in App (App (Var(add), App (Var(f), CInt(1))), App (Var(g), CInt(2)))) |}]
;;

let%expect_test "application chain with comparison" =
  print_string (print_ast (parse "f a b > g c d + 1" |> Result.get_ok));
  [%expect
    {| (App (App (Var(f), Var(a)), Var(b)) > (App (App (Var(g), Var(c)), Var(d)) + CInt(1))) |}]
;;

let%expect_test "nested lets with applications" =
  print_string
    (print_ast
       (parse "let x = 5 in let f = fun y -> x + y in let z = f 10 in z * 2"
        |> Result.get_ok));
  [%expect
    {| (let x = CInt(5) in (let f = Fun (Var(y), (Var(x) + Var(y))) in (let z = App (Var(f), CInt(10)) in (Var(z) * CInt(2))))) |}]
;;

let%expect_test "application with complex expressions" =
  print_string
    (print_ast (parse "f (x + 1) (y * 2) (if z > 0 then a else b)" |> Result.get_ok));
  [%expect
    {| App (App (App (Var(f), (Var(x) + CInt(1))), (Var(y) * CInt(2))), if ((Var(z) > CInt(0))) then (Var(a)) else (Var(b))) |}]
;;

let%expect_test "recursive function with application" =
  print_string
    (print_ast
       (parse "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5"
        |> Result.get_ok));
  [%expect
    {| (let rec fact = Fun (Var(n), if ((Var(n) = CInt(0))) then (CInt(1)) else ((Var(n) * App (Var(fact), (Var(n) - CInt(1)))))) in App (Var(fact), CInt(5))) |}]
;;

let%expect_test "right application" =
  print_string (print_ast (parse "g (f (q 5))" |> Result.get_ok));
  [%expect {| App (Var(g), App (Var(f), App (Var(q), CInt(5)))) |}]
;;

let%expect_test "nested if-then-else" =
  print_string
    (print_ast
       (parse "if 4>8 then if 7>9 then 7 else g 7 else g (k 4) 7" |> Result.get_ok));
  [%expect
    {| if ((CInt(4) > CInt(8))) then (if ((CInt(7) > CInt(9))) then (CInt(7)) else (App (Var(g), CInt(7)))) else (App (App (Var(g), App (Var(k), CInt(4))), CInt(7))) |}]
;;

let%expect_test "test else if" =
  print_string
    (print_ast (parse "if 4>8 then g 7 4 h else if 7>9 then 7 else g 7" |> Result.get_ok));
  [%expect
    {| if ((CInt(4) > CInt(8))) then (App (App (App (Var(g), CInt(7)), CInt(4)), Var(h))) else (if ((CInt(7) > CInt(9))) then (CInt(7)) else (App (Var(g), CInt(7)))) |}]
;;

(*--------------incorrect------------------
  let%expect_test "let in application" =
  assert (
  match parse "(let r = 4) 5" with
  | Ok _ -> false
  | Error _ -> true)
  ;;

  let%expect_test "let inside let" =
  assert (
  match parse "let = (let r = 4 + 5) 5" with
  | Ok _ -> false
  | Error _ -> true)
  ;;

  let%expect_test "let inside if" =
  assert (
  match parse "if let x = 5 in x > 0 then 1 else 0" with
  | Ok _ -> false
  | Error _ -> true)
  ;;

  let%expect_test "double let" =
  assert (
  match parse "let r = let x u= u + 7 in let q =7" with
  | Ok p ->
  let _ = Printf.printf "%s\n" (print_ast p) in
  false
  | Error _ -> true)
  ;;

  (*apparently check, whether it is an expression or not, is something that should be done in interpreter*)
  let%expect_test "if without else as app arg" =
  assert (
  match parse "r 5 (let f = 7 in 8)" with
  | Ok p ->
  let _ = Printf.printf "%s\n" (print_ast p) in
  false
  | Error _ -> true)
  ;;

  let%expect_test "let binding as app arg" =
  assert (
  match parse "r 5 (let r = 7)" with
  | Ok p ->
  let _ = Printf.printf "%s\n" (print_ast p) in
  false
  | Error _ -> true)
  ;;
  let _ =
  let r = fun s a -> s*a in
  r 5 (fun d -> d*d)

  let%expect_test "lambda as app arg" =
  assert (
  match parse "r 5 (fun r -> r*r)" with
  | Ok p ->
  let _ = Printf.printf "%s\n" (print_ast p) in
  false
  | Error _ -> true)
  ;;

  let%expect_test "if in binop" =
  assert (
  match parse "5 + (fun r -> r*r)" with
  | Ok p ->
  let _ = Printf.printf "%s\n" (print_ast p) in
  false
  | Error _ -> true)
  ;;

  else is obligatory now*)
(*how to deal with in after let - there is either in or ;;*)

(*okay, i should allow /\ (above) and check, whether is is an
  expression (return some value) inside interpreter
  but let = ... in let = g ;; - not allowed!!!
  /\
  || ????
  ||
  \/
  separate cases for expressions and statements?*)
