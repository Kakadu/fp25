[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

[@@@warning "-33"]

open Ast
open Interpret

let test_eval expr expected =
  match eval () expr with
  | Ok (VInt n) when n = expected -> true
  | Ok _ -> false
  | Error _ -> false
;;

let test_eval_error expr expected_error =
  match eval () expr with
  | Error err when err = expected_error -> true
  | _ -> false
;;

let%test "eval constant" = test_eval (Const 42) 42
let%test "eval addition" = test_eval (BinOp (Add, Const 2, Const 3)) 5
let%test "eval subtraction" = test_eval (BinOp (Sub, Const 10, Const 3)) 7
let%test "eval multiplication" = test_eval (BinOp (Mul, Const 4, Const 5)) 20
let%test "eval division" = test_eval (BinOp (Div, Const 20, Const 4)) 5

let%test "eval division by zero" =
  test_eval_error (BinOp (Div, Const 10, Const 0)) `DivisionByZero
;;

let%test "eval equality true" = test_eval (BinOp (Eq, Const 5, Const 5)) 1
let%test "eval equality false" = test_eval (BinOp (Eq, Const 5, Const 3)) 0
let%test "eval less than true" = test_eval (BinOp (Lt, Const 3, Const 5)) 1
let%test "eval less than false" = test_eval (BinOp (Lt, Const 5, Const 3)) 0
let%test "eval greater than true" = test_eval (BinOp (Gt, Const 5, Const 3)) 1
let%test "eval greater than false" = test_eval (BinOp (Gt, Const 3, Const 5)) 0
let%test "eval less or equal true" = test_eval (BinOp (Le, Const 5, Const 5)) 1
let%test "eval less or equal false" = test_eval (BinOp (Le, Const 5, Const 3)) 0
let%test "eval greater or equal true" = test_eval (BinOp (Ge, Const 5, Const 5)) 1
let%test "eval greater or equal false" = test_eval (BinOp (Ge, Const 3, Const 5)) 0
let%test "eval if true branch" = test_eval (If (Const 1, Const 10, Const 20)) 10
let%test "eval if false branch" = test_eval (If (Const 0, Const 10, Const 20)) 20
let%test "eval let binding" = test_eval (Let ("x", Const 5, Var "x")) 5

let%test "eval nested let" =
  test_eval (Let ("x", Const 5, Let ("y", Const 3, BinOp (Add, Var "x", Var "y")))) 8
;;

let%test "eval function application" = test_eval (App (Abs ("x", Var "x"), Const 42)) 42

let%test "eval closure" =
  test_eval
    (Let ("f", Abs ("x", BinOp (Add, Var "x", Const 1)), App (Var "f", Const 5)))
    6
;;

let%test "eval let rec factorial" =
  let fact_body =
    If
      ( BinOp (Gt, Var "n", Const 0)
      , BinOp (Mul, App (Var "fact", BinOp (Sub, Var "n", Const 1)), Var "n")
      , Const 1 )
  in
  test_eval (LetRec ("fact", "n", fact_body, App (Var "fact", Const 5))) 120
;;

let%test "eval unknown variable" = test_eval_error (Var "x") (`UnknownVariable "x")

let%test "eval type error in application" =
  match eval () (App (Const 5, Const 3)) with
  | Error (`TypeError _) -> true
  | _ -> false
;;

let%test "eval step limit" =
  let loop = LetRec ("loop", "x", App (Var "loop", Var "x"), App (Var "loop", Const 1)) in
  match eval ~step_limit:100 () loop with
  | Error `StepLimitReached -> true
  | _ -> false
;;

let%test "eval complex arithmetic" =
  test_eval
    (BinOp (Add, BinOp (Mul, Const 2, Const 3), BinOp (Sub, Const 10, Const 5)))
    11
;;

let%test "eval currying" =
  let add_fn = Abs ("x", Abs ("y", BinOp (Add, Var "x", Var "y"))) in
  test_eval (App (App (add_fn, Const 3), Const 5)) 8
;;

let%test "eval nested if" =
  test_eval (If (Const 1, If (Const 0, Const 10, Const 20), Const 30)) 20
;;

let%test "eval if with comparison" =
  test_eval (If (BinOp (Lt, Const 3, Const 5), Const 100, Const 200)) 100
;;

let%test "eval let with arithmetic" =
  test_eval (Let ("x", Const 10, BinOp (Mul, Var "x", Const 2))) 20
;;

let%test "eval shadowing" =
  test_eval (Let ("x", Const 5, Let ("x", Const 10, Var "x"))) 10
;;

let%test "eval multiple let bindings" =
  test_eval
    (Let
       ( "a"
       , Const 1
       , Let
           ( "b"
           , Const 2
           , Let ("c", Const 3, BinOp (Add, Var "a", BinOp (Add, Var "b", Var "c"))) ) ))
    6
;;

let%test "eval function composition" =
  let inc = Abs ("x", BinOp (Add, Var "x", Const 1)) in
  let double = Abs ("x", BinOp (Mul, Var "x", Const 2)) in
  test_eval
    (Let ("inc", inc, Let ("double", double, App (Var "inc", App (Var "double", Const 5)))))
    11
;;

let%test "eval higher order function" =
  let apply = Abs ("f", Abs ("x", App (Var "f", Var "x"))) in
  let inc = Abs ("y", BinOp (Add, Var "y", Const 1)) in
  test_eval
    (Let ("apply", apply, Let ("inc", inc, App (App (Var "apply", Var "inc"), Const 10))))
    11
;;

let%test "eval recursive sum" =
  let sum_body =
    If
      ( BinOp (Le, Var "n", Const 0)
      , Const 0
      , BinOp (Add, Var "n", App (Var "sum", BinOp (Sub, Var "n", Const 1))) )
  in
  test_eval (LetRec ("sum", "n", sum_body, App (Var "sum", Const 5))) 15
;;

let%test "eval recursive fibonacci" =
  let fib_body =
    If
      ( BinOp (Le, Var "n", Const 1)
      , Var "n"
      , BinOp
          ( Add
          , App (Var "fib", BinOp (Sub, Var "n", Const 1))
          , App (Var "fib", BinOp (Sub, Var "n", Const 2)) ) )
  in
  test_eval (LetRec ("fib", "n", fib_body, App (Var "fib", Const 6))) 8
;;

let%test "eval nested let rec" =
  let inner =
    LetRec ("inner", "x", BinOp (Add, Var "x", Const 1), App (Var "inner", Const 5))
  in
  test_eval (LetRec ("outer", "y", inner, App (Var "outer", Const 0))) 6
;;

let%test "eval all comparison operators" =
  test_eval (BinOp (Eq, Const 5, Const 5)) 1
  && test_eval (BinOp (Eq, Const 5, Const 3)) 0
  && test_eval (BinOp (Lt, Const 3, Const 5)) 1
  && test_eval (BinOp (Lt, Const 5, Const 3)) 0
  && test_eval (BinOp (Gt, Const 5, Const 3)) 1
  && test_eval (BinOp (Gt, Const 3, Const 5)) 0
  && test_eval (BinOp (Le, Const 3, Const 5)) 1
  && test_eval (BinOp (Le, Const 5, Const 3)) 0
  && test_eval (BinOp (Ge, Const 5, Const 3)) 1
  && test_eval (BinOp (Ge, Const 3, Const 5)) 0
;;

let%test "eval all arithmetic operators" =
  test_eval (BinOp (Add, Const 10, Const 5)) 15
  && test_eval (BinOp (Sub, Const 10, Const 5)) 5
  && test_eval (BinOp (Mul, Const 10, Const 5)) 50
  && test_eval (BinOp (Div, Const 10, Const 5)) 2
;;

let%test "eval operator precedence" =
  test_eval (BinOp (Add, Const 2, BinOp (Mul, Const 3, Const 4))) 14
;;

let%test "eval negative numbers" =
  test_eval (Const (-5)) (-5)
  && test_eval (BinOp (Add, Const (-3), Const 7)) 4
  && test_eval (BinOp (Sub, Const 0, Const 10)) (-10)
;;

let%test "eval zero" =
  test_eval (Const 0) 0
  && test_eval (BinOp (Mul, Const 100, Const 0)) 0
  && test_eval (BinOp (Add, Const 0, Const 0)) 0
;;

let%test "eval large numbers" =
  test_eval (BinOp (Add, Const 1000, Const 2000)) 3000
  && test_eval (BinOp (Mul, Const 100, Const 50)) 5000
;;

let%test "eval identity function" =
  test_eval (App (Abs ("x", Var "x"), Const 42)) 42
  && test_eval (Let ("id", Abs ("x", Var "x"), App (Var "id", Const 100))) 100
;;

let%test "eval const function" =
  let const_fn = Abs ("x", Abs ("y", Var "x")) in
  test_eval (App (App (const_fn, Const 10), Const 20)) 10
;;

let%test "eval nested closures" =
  let make_adder = Abs ("n", Abs ("x", BinOp (Add, Var "x", Var "n"))) in
  test_eval
    (Let
       ( "make_adder"
       , make_adder
       , Let ("add5", App (Var "make_adder", Const 5), App (Var "add5", Const 10)) ))
    15
;;

let%test "eval closure captures environment" =
  test_eval
    (Let
       ( "x"
       , Const 10
       , Let
           ( "f"
           , Abs ("y", BinOp (Add, Var "x", Var "y"))
           , Let ("x", Const 20, App (Var "f", Const 5)) ) ))
    15
;;

let%test "eval recursive even" =
  let even_body =
    If
      ( BinOp (Eq, Var "n", Const 0)
      , Const 1
      , If
          ( BinOp (Eq, Var "n", Const 1)
          , Const 0
          , App (Var "even", BinOp (Sub, Var "n", Const 2)) ) )
  in
  test_eval (LetRec ("even", "n", even_body, App (Var "even", Const 10))) 1
  && test_eval (LetRec ("even", "n", even_body, App (Var "even", Const 7))) 0
;;

let%test "eval if with nested comparisons" =
  test_eval (If (BinOp (Lt, BinOp (Add, Const 2, Const 3), Const 10), Const 1, Const 0)) 1
;;

let%test "eval complex expression" =
  test_eval
    (Let
       ( "double"
       , Abs ("x", BinOp (Mul, Var "x", Const 2))
       , Let
           ( "triple"
           , Abs ("y", BinOp (Mul, Var "y", Const 3))
           , BinOp (Add, App (Var "double", Const 5), App (Var "triple", Const 4)) ) ))
    22
;;

let%test "eval chained let bindings" =
  test_eval
    (Let
       ( "a"
       , Const 1
       , Let
           ( "b"
           , BinOp (Add, Var "a", Const 1)
           , Let ("c", BinOp (Add, Var "b", Const 1), Var "c") ) ))
    3
;;

let%test "eval division by zero in expression" =
  test_eval_error (Let ("x", BinOp (Div, Const 10, Const 0), Var "x")) `DivisionByZero
;;

let%test "eval unknown variable in nested let" =
  test_eval_error (Let ("x", Const 5, Var "y")) (`UnknownVariable "y")
;;

let%test "eval unknown variable in function body" =
  test_eval_error (App (Abs ("x", Var "y"), Const 5)) (`UnknownVariable "y")
;;

let%test "eval type error: if condition non-int" =
  match eval () (If (Abs ("x", Var "x"), Const 1, Const 0)) with
  | Error (`TypeError _) -> true
  | _ -> false
;;

let%test "eval type error: binop with function" =
  match eval () (BinOp (Add, Abs ("x", Var "x"), Const 5)) with
  | Error (`TypeError _) -> true
  | _ -> false
;;

let%test "eval type error: applying non-function" =
  match eval () (App (Const 42, Const 10)) with
  | Error (`TypeError _) -> true
  | _ -> false
;;

let%test "eval step limit reached with loop" =
  let loop = LetRec ("loop", "x", App (Var "loop", Var "x"), App (Var "loop", Const 1)) in
  match eval ~step_limit:50 () loop with
  | Error `StepLimitReached -> true
  | _ -> false
;;

let%test "eval step limit with recursive sum" =
  let sum_body =
    If
      ( BinOp (Le, Var "n", Const 0)
      , Const 0
      , BinOp (Add, Var "n", App (Var "sum", BinOp (Sub, Var "n", Const 1))) )
  in
  match
    eval ~step_limit:10 () (LetRec ("sum", "n", sum_body, App (Var "sum", Const 100)))
  with
  | Error `StepLimitReached -> true
  | _ -> false
;;

let%test "eval with large step limit" =
  let fact_body =
    If
      ( BinOp (Gt, Var "n", Const 0)
      , BinOp (Mul, App (Var "fact", BinOp (Sub, Var "n", Const 1)), Var "n")
      , Const 1 )
  in
  match
    eval
      ~step_limit:10000
      ()
      (LetRec ("fact", "n", fact_body, App (Var "fact", Const 10)))
  with
  | Ok (VInt n) when n = 3628800 -> true
  | _ -> false
;;

let%test "eval curried three arguments" =
  let add3 =
    Abs ("x", Abs ("y", Abs ("z", BinOp (Add, Var "x", BinOp (Add, Var "y", Var "z")))))
  in
  test_eval (App (App (App (add3, Const 1), Const 2), Const 3)) 6
;;

let%test "eval function returning function" =
  let maker = Abs ("x", Abs ("y", BinOp (Mul, Var "x", Var "y"))) in
  test_eval
    (Let
       ( "maker"
       , maker
       , Let ("mul5", App (Var "maker", Const 5), App (Var "mul5", Const 7)) ))
    35
;;

let%test "eval comparison chain" =
  test_eval (BinOp (Lt, BinOp (Add, Const 1, Const 2), BinOp (Mul, Const 2, Const 3))) 1
;;

let%test "eval min function" =
  let min_body = If (BinOp (Lt, Var "a", Var "b"), Var "a", Var "b") in
  test_eval
    (Let ("min", Abs ("a", Abs ("b", min_body)), App (App (Var "min", Const 5), Const 10)))
    5
;;

let%test "eval max function" =
  let max_body = If (BinOp (Gt, Var "a", Var "b"), Var "a", Var "b") in
  test_eval
    (Let ("max", Abs ("a", Abs ("b", max_body)), App (App (Var "max", Const 5), Const 10)))
    10
;;

let%test "eval abs function" =
  let abs_body =
    If (BinOp (Lt, Var "x", Const 0), BinOp (Sub, Const 0, Var "x"), Var "x")
  in
  test_eval (Let ("abs", Abs ("x", abs_body), App (Var "abs", Const (-5)))) 5
  && test_eval (Let ("abs", Abs ("x", abs_body), App (Var "abs", Const 5))) 5
;;
