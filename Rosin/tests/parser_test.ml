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
let pp = Pprintast.pp

let%expect_test "number" =
  Format.printf "%a" pp (parse_optimistically "42");
  [%expect{| Int(42) |}]
;;
let%expect_test "negative number" =
  Format.printf "%a" pp (parse_optimistically "-42");
  [%expect{| Int(-42) |}]
;;

let%expect_test "variable" =
  Format.printf "%a" pp (parse_optimistically "x");
  [%expect{| Var(x) |}]
;;

let%expect_test "number sum" =
  Format.printf "%a" pp (parse_optimistically "2 -2");
  [%expect{| (App(Int(2), Int(-2))) |}]
;;

let%expect_test "number and variable sum" =
  Format.printf "%a" pp (parse_optimistically "x + 2");
  [%expect{| (Plus(Var(x), Int(2))) |}]
;;

let%expect_test "binary operation chain with multiplication" =
  Format.printf "%a" pp (parse_optimistically "y + 2 * x");
  [%expect{| (Plus(Var(y), (Mult(Int(2), Var(x))))) |}]
;;

let%expect_test "binary operation chain with division" =
  Format.printf "%a" pp (parse_optimistically "y + 2 / x");
  [%expect{| (Plus(Var(y), (Div(Int(2), Var(x))))) |}]
;;

let%expect_test "number increment" =
  Format.printf "%a" pp (parse_optimistically "++2");
  [%expect{| (Inc(Int(2))) |}]
;;

let%expect_test "variable decrement" =
  Format.printf "%a" pp (parse_optimistically "--x");
  [%expect{| (Dec(Var(x))) |}]
;;

let%expect_test "variable increment with binary operation" =
  Format.printf "%a" pp (parse_optimistically "++x + 2");
  [%expect{| (Plus((Inc(Var(x))), Int(2))) |}]
;;

let%expect_test "easy condition" =
  Format.printf "%a" pp (parse_optimistically "if x-2 then x else -2");
  [%expect{| If(((App(Var(x), Int(-2)))) Then(Var(x)) Else(Int(-2))) |}]
;;

let%expect_test "nested condition" =
  Format.printf "%a" pp (parse_optimistically "if x-2 then if(x+3) then 3 else 1 else -2");
  [%expect{| If(((App(Var(x), Int(-2)))) Then(If(((Plus(Var(x), Int(3)))) Then(Int(3)) Else(Int(1)))) Else(Int(-2))) |}]
;;

let%expect_test "easy anonimous function" =
  Format.printf "%a" pp (parse_optimistically "fun x -> x+2");
  [%expect{| Fun(x, (Plus(Var(x), Int(2)))) |}]
;;

let%expect_test "anonimous function with condition in body" =
  Format.printf "%a" pp (parse_optimistically "fun x -> if(x-2) then 2 else x");
  [%expect{| Fun(x, If(((App(Var(x), Int(-2)))) Then(Int(2)) Else(Var(x)))) |}]
;;

let%expect_test "anonimous function with few variables" =
  Format.printf "%a" pp (parse_optimistically "fun x y -> if(x + y + 2) then 2 else x + y");
  [%expect{| Fun(x, Fun(y, If(((Plus((Plus(Var(x), Var(y))), Int(2)))) Then(Int(2)) Else((Plus(Var(x), Var(y))))))) |}]
;;

let%expect_test "anonimous function with condition in body" =
  Format.printf "%a" pp (parse_optimistically "fun x -> if(x+2) then 2 else x");
  [%expect{| Fun(x, If(((Plus(Var(x), Int(2)))) Then(Int(2)) Else(Var(x)))) |}]
;;

let%expect_test "easy let" =
  Format.printf "%a" pp (parse_optimistically "let x = 2 in x + 2");
  [%expect{| Let((x, Int(2)) in (Plus(Var(x), Int(2)))) |}]
;;

let%expect_test "let for function defenition" =
  Format.printf "%a" pp (parse_optimistically "let sub x = x - 1");
  [%expect{| Let(sub, Fun(x, (Minus(Var(x), Int(1))))) |}]
;;

let%expect_test "function application" =
  Format.printf "%a" pp (parse_optimistically "sub x");
  [%expect{| (App(Var(sub), Var(x))) |}]
;;