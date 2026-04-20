[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Ast = Mini_ml_lib.Ast

let wildcard = Ast.Wildcard
let name name = Ast.Real name
let var name = Ast.Var (Ast.Real name)
let const value = Ast.Int value
let bool value = Ast.Bool value
let neg expr = Ast.UnaryOp (Ast.Neg, expr)
let not expr = Ast.UnaryOp (Ast.Not, expr)
let add lhs rhs = Ast.BinaryOp (Ast.Add, lhs, rhs)
let sub lhs rhs = Ast.BinaryOp (Ast.Sub, lhs, rhs)
let mul lhs rhs = Ast.BinaryOp (Ast.Mul, lhs, rhs)
let div lhs rhs = Ast.BinaryOp (Ast.Div, lhs, rhs)
let rem lhs rhs = Ast.BinaryOp (Ast.Mod, lhs, rhs)

let expect ast verbose pretty =
  Ast.show_ast_verbose ast = verbose && Ast.show_ast ast = pretty
;;

let%test "Unit" = expect Ast.Unit "unit" "()"
let%test "Variable" = expect (var "someVariable") "someVariable" "someVariable"

let%test "Arithmetic 1" =
  expect
    (add (add (const 123) (const 456)) (const 789))
    "Add(Add(123, 456), 789)"
    "((123 + 456) + 789)"
;;

let%test "Arithmetic 2" =
  expect
    (neg (mul (const 2) (div (var "y") (var "x"))))
    "Neg(Mul(2, Div(y, x)))"
    "-((2 * (y / x)))"
;;

let%test "Arithmetic 3" =
  expect
    (rem (const 44) (sub (var "y") (const 1)))
    "Mod(44, Sub(y, 1))"
    "(44 mod (y - 1))"
;;

let eq lhs rhs = Ast.BinaryOp (Ast.Equal, lhs, rhs)
let ne lhs rhs = Ast.BinaryOp (Ast.NotEqual, lhs, rhs)
let lt lhs rhs = Ast.BinaryOp (Ast.Less, lhs, rhs)
let le lhs rhs = Ast.BinaryOp (Ast.LessEqual, lhs, rhs)
let gt lhs rhs = Ast.BinaryOp (Ast.Greater, lhs, rhs)
let ge lhs rhs = Ast.BinaryOp (Ast.GreaterEqual, lhs, rhs)
let ast_and lhs rhs = Ast.BinaryOp (Ast.And, lhs, rhs)
let ast_or lhs rhs = Ast.BinaryOp (Ast.Or, lhs, rhs)

let%test "Bool 1" =
  expect (ast_or (bool true) (bool false)) "Or(true, false)" "(true || false)"
;;

let%test "Bool 2" =
  expect
    (not (eq (le (var "a0") (var "a1")) (var "b")))
    "Not(Equal(LessEqual(a0, a1), b))"
    "!(((a0 <= a1) = b))"
;;

let%test "Bool 3" =
  expect
    (gt (const 42) (ne (var "flag") (bool false)))
    "Greater(42, NotEqual(flag, false))"
    "(42 > (flag <> false))"
;;

let%test "Bool 4" =
  expect
    (ast_and (ast_or (lt (const 12) (const 24)) (var "x")) (ge (var "y") (var "z")))
    "And(Or(Less(12, 24), x), GreaterEqual(y, z))"
    "(((12 < 24) || x) && (y >= z))"
;;

let app func arg = Ast.Application (func, arg)
let abs arg expr = Ast.Abstraction (arg, expr)

let%test "Applications & abstractions" =
  expect
    (app (app (abs wildcard (abs (name "a") (const 12))) (var "x1")) (var "x2"))
    "App(App(Abs(_, Abs(a, 12)), x1), x2)"
    "((((fun _ -> fun a -> 12) (x1))) (x2))"
;;

let declare_excepttion name = Ast.Exception name
let try_with left name right = Ast.TryWith (left, name, right)
let raise expr = Ast.Raise expr

let%test "Exceptions 1" =
  expect
    (try_with
       (app (var "print_int") (const 4242))
       "VeryBadCase"
       (app (var "print_bool") (bool false)))
    "TryWith(App(print_int, 4242), VeryBadCase, App(print_bool, false))"
    "try (((print_int) (4242))) with VeryBadCase -> (((print_bool) (false)))"
;;

let%test "Exceptions 2" =
  expect (raise (abs (name "x") (bool true))) "Raise(Abs(x, true))" "raise fun x -> true"
;;

let%test "Exceptions 3" =
  expect
    (declare_excepttion "DivisionByZero")
    "Exception(DivisionByZero)"
    "exception DivisionByZero"
;;
