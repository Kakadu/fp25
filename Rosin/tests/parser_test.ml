[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib
open Parser

let parse_optimistically str = Result.get_ok (parse str)

let parse_pessimistically str =
  match parse str with
  | Error _ -> true
  | _ -> false
;;

let pp = Pprintast.pp

let%expect_test "number" =
  Format.printf "%a" pp (parse_optimistically "42");
  [%expect {| Int(42) |}]
;;

let%expect_test "negative number" =
  Format.printf "%a" pp (parse_optimistically "-42");
  [%expect {| Int(-42) |}]
;;

let%expect_test "variable" =
  Format.printf "%a" pp (parse_optimistically "x");
  [%expect {| Var(x) |}]
;;

let%expect_test "variable" =
  Format.printf
    "%a"
    pp
    (parse_optimistically "let rec infinite = fun x -> infinite x in infinite 1");
  [%expect
    {| Letrec((infinite, Fun(x, (App(Var(infinite), Var(x))))) in (App(Var(infinite), Int(1)))) |}]
;;

let%expect_test "variable" =
  Format.printf
    "%a"
    pp
    (parse_optimistically
       "let fact = fix (fun f n -> if n then n * f (n - 1) else 1) in fact 5");
  [%expect
    {| Let(fact, Fix(Fun(f, Fun(n, If(Var(n)) Then((Mult(Var(n), (App(Var(f), (Minus(Var(n), Int(1)))))))) Else (Int(1))))))) in (App(Var(fact), Int(5))) |}]
;;

let%expect_test "number sum" =
  Format.printf "%a" pp (parse_optimistically "2 -2");
  [%expect {| (Minus(Int(2), Int(2))) |}]
;;

let%expect_test "number and variable sum" =
  Format.printf "%a" pp (parse_optimistically "x - 2");
  [%expect {| (Minus(Var(x), Int(2))) |}]
;;

let%expect_test "binary operation chain with multiplication" =
  Format.printf "%a" pp (parse_optimistically "y + 2 * x");
  [%expect {| (Plus(Var(y), (Mult(Int(2), Var(x))))) |}]
;;

let%expect_test "binary operation chain with division" =
  Format.printf "%a" pp (parse_optimistically "y + 2 / x");
  [%expect {| (Plus(Var(y), (Div(Int(2), Var(x))))) |}]
;;

let%expect_test "number increment" =
  Format.printf "%a" pp (parse_optimistically "++2");
  [%expect {| (Inc(Int(2))) |}]
;;

let%expect_test "variable decrement" =
  Format.printf "%a" pp (parse_optimistically "--x");
  [%expect {| (Dec(Var(x))) |}]
;;

let%expect_test "variable increment with binary operation" =
  Format.printf "%a" pp (parse_optimistically "(++x) + 2");
  [%expect {| (Plus((Inc(Var(x))), Int(2))) |}]
;;

let%expect_test "easy condition" =
  Format.printf "%a" pp (parse_optimistically "if x - 2 then x else -2");
  [%expect {| If((Minus(Var(x), Int(2)))) Then(Var(x)) Else (Int(-2))) |}]
;;

let%expect_test "nested condition" =
  Format.printf
    "%a"
    pp
    (parse_optimistically "if x - 2 then if(x + 3) then 3 else 1 else -2");
  [%expect
    {| If((Minus(Var(x), Int(2)))) Then(If((Plus(Var(x), Int(3)))) Then(Int(3)) Else (Int(1)))) Else (Int(-2))) |}]
;;

let%expect_test "easy anonimous function" =
  Format.printf "%a" pp (parse_optimistically "fun x -> x + 2");
  [%expect {| Fun(x, (Plus(Var(x), Int(2)))) |}]
;;

let%expect_test "anonimous function with condition in body" =
  Format.printf "%a" pp (parse_optimistically "fun x -> if(x - 2) then 2 else x");
  [%expect {| Fun(x, If((Minus(Var(x), Int(2)))) Then(Int(2)) Else (Var(x)))) |}]
;;

let%expect_test "anonimous function with few variables" =
  Format.printf
    "%a"
    pp
    (parse_optimistically "fun x y -> if(x + y + 2) then 2 else x + y");
  [%expect
    {| Fun(x, Fun(y, If((Plus((Plus(Var(x), Var(y))), Int(2)))) Then(Int(2)) Else ((Plus(Var(x), Var(y))))))) |}]
;;

let%expect_test "anonimous function with condition in body" =
  Format.printf "%a" pp (parse_optimistically "fun x -> if(x + 2) then 2 else x");
  [%expect {| Fun(x, If((Plus(Var(x), Int(2)))) Then(Int(2)) Else (Var(x)))) |}]
;;

let%expect_test "easy let" =
  Format.printf "%a" pp (parse_optimistically "let x = 2 in x + 2");
  [%expect {| Let(x, Int(2)) in (Plus(Var(x), Int(2))) |}]
;;

let%expect_test "let for function defenition" =
  Format.printf "%a" pp (parse_optimistically "let sub x = x - 1 in sub 2");
  [%expect {| Let(sub, Fun(x, (Minus(Var(x), Int(1))))) in (App(Var(sub), Int(2))) |}]
;;

let%expect_test "function application" =
  Format.printf "%a" pp (parse_optimistically "sub x");
  [%expect {| (App(Var(sub), Var(x))) |}]
;;

let%expect_test "fib" =
  Format.printf
    "%a"
    pp
    (parse_optimistically
       "let rec fib = fun n -> if n - 1 then n else fib (n - 1) + fib (n + 2) in fib 6");
  [%expect
    {| Letrec((fib, Fun(n, If((Minus(Var(n), Int(1)))) Then(Var(n)) Else ((Plus((App(Var(fib), (Minus(Var(n), Int(1))))), (App(Var(fib), (Plus(Var(n), Int(2))))))))))) in (App(Var(fib), Int(6)))) |}]
;;

let%expect_test "factorial of 5" =
  let fact_program =
    "let rec fact = fun n ->\n\
    \      if 2-n then 1 else n * fact (n - 1)\n\
    \    in\n\
    \    fact 2"
  in
  Format.printf "%a" pp (parse_optimistically fact_program);
  [%expect
    {| Letrec((fact, Fun(n, If((Minus(Int(2), Var(n)))) Then(Int(1)) Else ((Mult(Var(n), (App(Var(fact), (Minus(Var(n), Int(1))))))))))) in (App(Var(fact), Int(2)))) |}]
;;

let%expect_test "fibonacci of 6" =
  let fib_program =
    "\n\
    \    let rec fib = fun n ->\n\
    \      if 2-n then n else fib (n - 1) + fib (n - 2)\n\
    \    in\n\
    \    fib 6\n\
    \  "
  in
  Format.printf "%a" pp (parse_optimistically fib_program);
  [%expect
    {| Letrec((fib, Fun(n, If((Minus(Int(2), Var(n)))) Then(Var(n)) Else ((Plus((App(Var(fib), (Minus(Var(n), Int(1))))), (App(Var(fib), (Minus(Var(n), Int(2))))))))))) in (App(Var(fib), Int(6)))) |}]
;;

let%expect_test "print int" =
  Format.printf "%a" pp (parse_optimistically "print 42");
  [%expect {| Print(Int(42)) |}]
;;

let%expect_test "print variable" =
  Format.printf "%a" pp (parse_optimistically "let x = 42 in print x");
  [%expect {|
    Let(x, Int(42)) in Print(Var(x)) |}]
;;
