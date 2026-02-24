[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

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

let%test "eval constant" = test_eval (Ast.Const 42) 42
let%test "eval addition" = test_eval (Ast.BinOp (Ast.Add, Ast.Const 2, Ast.Const 3)) 5
let%test "eval subtraction" = test_eval (Ast.BinOp (Ast.Sub, Ast.Const 10, Ast.Const 3)) 7

let%test "eval multiplication" =
  test_eval (Ast.BinOp (Ast.Mul, Ast.Const 4, Ast.Const 5)) 20
;;

let%test "eval division" = test_eval (Ast.BinOp (Ast.Div, Ast.Const 20, Ast.Const 4)) 5

let%test "eval division by zero" =
  test_eval_error (Ast.BinOp (Ast.Div, Ast.Const 10, Ast.Const 0)) `DivisionByZero
;;

let%test "eval equality true" = test_eval (Ast.BinOp (Ast.Eq, Ast.Const 5, Ast.Const 5)) 1
let%test "eval equality false" = test_eval (Ast.BinOp (Ast.Eq, Ast.Const 5, Ast.Const 3)) 0
let%test "eval less than true" = test_eval (Ast.BinOp (Ast.Lt, Ast.Const 3, Ast.Const 5)) 1
let%test "eval less than false" = test_eval (Ast.BinOp (Ast.Lt, Ast.Const 5, Ast.Const 3)) 0
let%test "eval greater than true" = test_eval (Ast.BinOp (Ast.Gt, Ast.Const 5, Ast.Const 3)) 1
let%test "eval greater than false" = test_eval (Ast.BinOp (Ast.Gt, Ast.Const 3, Ast.Const 5)) 0
let%test "eval less or equal true" = test_eval (Ast.BinOp (Ast.Le, Ast.Const 5, Ast.Const 5)) 1
let%test "eval less or equal false" = test_eval (Ast.BinOp (Ast.Le, Ast.Const 5, Ast.Const 3)) 0
let%test "eval greater or equal true" = test_eval (Ast.BinOp (Ast.Ge, Ast.Const 5, Ast.Const 5)) 1
let%test "eval greater or equal false" = test_eval (Ast.BinOp (Ast.Ge, Ast.Const 3, Ast.Const 5)) 0
let%test "eval if true branch" = test_eval (Ast.If (Ast.Const 1, Ast.Const 10, Ast.Const 20)) 10
let%test "eval if false branch" = test_eval (Ast.If (Ast.Const 0, Ast.Const 10, Ast.Const 20)) 20
let%test "eval let binding" = test_eval (Ast.Let ("x", Ast.Const 5, Ast.Var "x")) 5

let%test "eval nested let" =
  test_eval (Ast.Let ("x", Ast.Const 5, Ast.Let ("y", Ast.Const 3, Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Var "y")))) 8
;;

let%test "eval function application" = test_eval (Ast.App (Ast.Abs ("x", Ast.Var "x"), Ast.Const 42)) 42

let%test "eval closure" =
  test_eval
    (Ast.Let ("f", Ast.Abs ("x", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Const 1)), Ast.App (Ast.Var "f", Ast.Const 5)))
    6
;;

let%test "eval let rec factorial" =
  let fact_body =
    Ast.If
      ( Ast.BinOp (Ast.Gt, Ast.Var "n", Ast.Const 0)
      , Ast.BinOp (Ast.Mul, Ast.App (Ast.Var "fact", Ast.BinOp (Ast.Sub, Ast.Var "n", Ast.Const 1)), Ast.Var "n")
      , Ast.Const 1 )
  in
  test_eval (Ast.LetRec ("fact", "n", fact_body, Ast.App (Ast.Var "fact", Ast.Const 5))) 120
;;

let%test "eval unknown variable" = test_eval_error (Ast.Var "x") (`UnknownVariable "x")

let%test "eval type error in application" =
  match eval () (Ast.App (Ast.Const 5, Ast.Const 3)) with
  | Error (`TypeError _) -> true
  | _ -> false
;;

let%test "eval step limit" =
  let loop = Ast.LetRec ("loop", "x", Ast.App (Ast.Var "loop", Ast.Var "x"), Ast.App (Ast.Var "loop", Ast.Const 1)) in
  match eval ~step_limit:100 () loop with
  | Error `StepLimitReached -> true
  | _ -> false
;;

let%test "eval complex arithmetic" =
  test_eval
    (Ast.BinOp (Ast.Add, Ast.BinOp (Ast.Mul, Ast.Const 2, Ast.Const 3), Ast.BinOp (Ast.Sub, Ast.Const 10, Ast.Const 5)))
    11
;;

let%test "eval currying" =
  let add_fn = Ast.Abs ("x", Ast.Abs ("y", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Var "y"))) in
  test_eval (Ast.App (Ast.App (add_fn, Ast.Const 3), Ast.Const 5)) 8
;;

let%test "eval nested if" =
  test_eval (Ast.If (Ast.Const 1, Ast.If (Ast.Const 0, Ast.Const 10, Ast.Const 20), Ast.Const 30)) 20
;;

let%test "eval if with comparison" =
  test_eval (Ast.If (Ast.BinOp (Ast.Lt, Ast.Const 3, Ast.Const 5), Ast.Const 100, Ast.Const 200)) 100
;;

let%test "eval let with arithmetic" =
  test_eval (Ast.Let ("x", Ast.Const 10, Ast.BinOp (Ast.Mul, Ast.Var "x", Ast.Const 2))) 20
;;

let%test "eval shadowing" =
  test_eval (Ast.Let ("x", Ast.Const 5, Ast.Let ("x", Ast.Const 10, Ast.Var "x"))) 10
;;

let%test "eval multiple let bindings" =
  test_eval
    (Ast.Let
       ( "a"
       , Ast.Const 1
       , Ast.Let
           ( "b"
           , Ast.Const 2
           , Ast.Let ("c", Ast.Const 3, Ast.BinOp (Ast.Add, Ast.Var "a", Ast.BinOp (Ast.Add, Ast.Var "b", Ast.Var "c"))) ) ))
    6
;;

let%test "eval function composition" =
  let inc = Ast.Abs ("x", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Const 1)) in
  let double = Ast.Abs ("x", Ast.BinOp (Ast.Mul, Ast.Var "x", Ast.Const 2)) in
  test_eval
    (Ast.Let ("inc", inc, Ast.Let ("double", double, Ast.App (Ast.Var "inc", Ast.App (Ast.Var "double", Ast.Const 5)))))
    11
;;

let%test "eval higher order function" =
  let apply = Ast.Abs ("f", Ast.Abs ("x", Ast.App (Ast.Var "f", Ast.Var "x"))) in
  let inc = Ast.Abs ("y", Ast.BinOp (Ast.Add, Ast.Var "y", Ast.Const 1)) in
  test_eval
    (Ast.Let ("apply", apply, Ast.Let ("inc", inc, Ast.App (Ast.App (Ast.Var "apply", Ast.Var "inc"), Ast.Const 10))))
    11
;;

let%test "eval recursive sum" =
  let sum_body =
    Ast.If
      ( Ast.BinOp (Ast.Le, Ast.Var "n", Ast.Const 0)
      , Ast.Const 0
      , Ast.BinOp (Ast.Add, Ast.Var "n", Ast.App (Ast.Var "sum", Ast.BinOp (Ast.Sub, Ast.Var "n", Ast.Const 1))) )
  in
  test_eval (Ast.LetRec ("sum", "n", sum_body, Ast.App (Ast.Var "sum", Ast.Const 5))) 15
;;

let%test "eval recursive fibonacci" =
  let fib_body =
    Ast.If
      ( Ast.BinOp (Ast.Le, Ast.Var "n", Ast.Const 1)
      , Ast.Var "n"
      , Ast.BinOp
          ( Ast.Add
          , Ast.App (Ast.Var "fib", Ast.BinOp (Ast.Sub, Ast.Var "n", Ast.Const 1))
          , Ast.App (Ast.Var "fib", Ast.BinOp (Ast.Sub, Ast.Var "n", Ast.Const 2)) ) )
  in
  test_eval (Ast.LetRec ("fib", "n", fib_body, Ast.App (Ast.Var "fib", Ast.Const 6))) 8
;;

let%test "eval nested let rec" =
  let inner =
    Ast.LetRec ("inner", "x", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Const 1), Ast.App (Ast.Var "inner", Ast.Const 5))
  in
  test_eval (Ast.LetRec ("outer", "y", inner, Ast.App (Ast.Var "outer", Ast.Const 0))) 6
;;

let%test "eval all comparison operators" =
  test_eval (Ast.BinOp (Ast.Eq, Ast.Const 5, Ast.Const 5)) 1
  && test_eval (Ast.BinOp (Ast.Eq, Ast.Const 5, Ast.Const 3)) 0
  && test_eval (Ast.BinOp (Ast.Lt, Ast.Const 3, Ast.Const 5)) 1
  && test_eval (Ast.BinOp (Ast.Lt, Ast.Const 5, Ast.Const 3)) 0
  && test_eval (Ast.BinOp (Ast.Gt, Ast.Const 5, Ast.Const 3)) 1
  && test_eval (Ast.BinOp (Ast.Gt, Ast.Const 3, Ast.Const 5)) 0
  && test_eval (Ast.BinOp (Ast.Le, Ast.Const 3, Ast.Const 5)) 1
  && test_eval (Ast.BinOp (Ast.Le, Ast.Const 5, Ast.Const 3)) 0
  && test_eval (Ast.BinOp (Ast.Ge, Ast.Const 5, Ast.Const 3)) 1
  && test_eval (Ast.BinOp (Ast.Ge, Ast.Const 3, Ast.Const 5)) 0
;;

let%test "eval all arithmetic operators" =
  test_eval (Ast.BinOp (Ast.Add, Ast.Const 10, Ast.Const 5)) 15
  && test_eval (Ast.BinOp (Ast.Sub, Ast.Const 10, Ast.Const 5)) 5
  && test_eval (Ast.BinOp (Ast.Mul, Ast.Const 10, Ast.Const 5)) 50
  && test_eval (Ast.BinOp (Ast.Div, Ast.Const 10, Ast.Const 5)) 2
;;

let%test "eval operator precedence" =
  test_eval (Ast.BinOp (Ast.Add, Ast.Const 2, Ast.BinOp (Ast.Mul, Ast.Const 3, Ast.Const 4))) 14
;;

let%test "eval negative numbers" =
  test_eval (Ast.Const (-5)) (-5)
  && test_eval (Ast.BinOp (Ast.Add, Ast.Const (-3), Ast.Const 7)) 4
  && test_eval (Ast.BinOp (Ast.Sub, Ast.Const 0, Ast.Const 10)) (-10)
;;

let%test "eval zero" =
  test_eval (Ast.Const 0) 0
  && test_eval (Ast.BinOp (Ast.Mul, Ast.Const 100, Ast.Const 0)) 0
  && test_eval (Ast.BinOp (Ast.Add, Ast.Const 0, Ast.Const 0)) 0
;;

let%test "eval large numbers" =
  test_eval (Ast.BinOp (Ast.Add, Ast.Const 1000, Ast.Const 2000)) 3000
  && test_eval (Ast.BinOp (Ast.Mul, Ast.Const 100, Ast.Const 50)) 5000
;;

let%test "eval identity function" =
  test_eval (Ast.App (Ast.Abs ("x", Ast.Var "x"), Ast.Const 42)) 42
  && test_eval (Ast.Let ("id", Ast.Abs ("x", Ast.Var "x"), Ast.App (Ast.Var "id", Ast.Const 100))) 100
;;

let%test "eval const function" =
  let const_fn = Ast.Abs ("x", Ast.Abs ("y", Ast.Var "x")) in
  test_eval (Ast.App (Ast.App (const_fn, Ast.Const 10), Ast.Const 20)) 10
;;

let%test "eval nested closures" =
  let make_adder = Ast.Abs ("n", Ast.Abs ("x", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Var "n"))) in
  test_eval
    (Ast.Let
       ( "make_adder"
       , make_adder
       , Ast.Let ("add5", Ast.App (Ast.Var "make_adder", Ast.Const 5), Ast.App (Ast.Var "add5", Ast.Const 10)) ))
    15
;;

let%test "eval closure captures environment" =
  test_eval
    (Ast.Let
       ( "x"
       , Ast.Const 10
       , Ast.Let
           ( "f"
           , Ast.Abs ("y", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Var "y"))
           , Ast.Let ("x", Ast.Const 20, Ast.App (Ast.Var "f", Ast.Const 5)) ) ))
    15
;;

let%test "eval recursive even" =
  let even_body =
    Ast.If
      ( Ast.BinOp (Ast.Eq, Ast.Var "n", Ast.Const 0)
      , Ast.Const 1
      , Ast.If
          ( Ast.BinOp (Ast.Eq, Ast.Var "n", Ast.Const 1)
          , Ast.Const 0
          , Ast.App (Ast.Var "even", Ast.BinOp (Ast.Sub, Ast.Var "n", Ast.Const 2)) ) )
  in
  test_eval (Ast.LetRec ("even", "n", even_body, Ast.App (Ast.Var "even", Ast.Const 10))) 1
  && test_eval (Ast.LetRec ("even", "n", even_body, Ast.App (Ast.Var "even", Ast.Const 7))) 0
;;

let%test "eval if with nested comparisons" =
  test_eval (Ast.If (Ast.BinOp (Ast.Lt, Ast.BinOp (Ast.Add, Ast.Const 2, Ast.Const 3), Ast.Const 10), Ast.Const 1, Ast.Const 0)) 1
;;

let%test "eval complex expression" =
  test_eval
    (Ast.Let
       ( "double"
       , Ast.Abs ("x", Ast.BinOp (Ast.Mul, Ast.Var "x", Ast.Const 2))
       , Ast.Let
           ( "triple"
           , Ast.Abs ("y", Ast.BinOp (Ast.Mul, Ast.Var "y", Ast.Const 3))
           , Ast.BinOp (Ast.Add, Ast.App (Ast.Var "double", Ast.Const 5), Ast.App (Ast.Var "triple", Ast.Const 4)) ) ))
    22
;;

let%test "eval chained let bindings" =
  test_eval
    (Ast.Let
       ( "a"
       , Ast.Const 1
       , Ast.Let
           ( "b"
           , Ast.BinOp (Ast.Add, Ast.Var "a", Ast.Const 1)
           , Ast.Let ("c", Ast.BinOp (Ast.Add, Ast.Var "b", Ast.Const 1), Ast.Var "c") ) ))
    3
;;

let%test "eval division by zero in expression" =
  test_eval_error (Ast.Let ("x", Ast.BinOp (Ast.Div, Ast.Const 10, Ast.Const 0), Ast.Var "x")) `DivisionByZero
;;

let%test "eval unknown variable in nested let" =
  test_eval_error (Ast.Let ("x", Ast.Const 5, Ast.Var "y")) (`UnknownVariable "y")
;;

let%test "eval unknown variable in function body" =
  test_eval_error (Ast.App (Ast.Abs ("x", Ast.Var "y"), Ast.Const 5)) (`UnknownVariable "y")
;;

let%test "eval type error: if condition non-int" =
  match eval () (Ast.If (Ast.Abs ("x", Ast.Var "x"), Ast.Const 1, Ast.Const 0)) with
  | Error (`TypeError _) -> true
  | _ -> false
;;

let%test "eval type error: binop with function" =
  match eval () (Ast.BinOp (Ast.Add, Ast.Abs ("x", Ast.Var "x"), Ast.Const 5)) with
  | Error (`TypeError _) -> true
  | _ -> false
;;

let%test "eval type error: applying non-function" =
  match eval () (Ast.App (Ast.Const 42, Ast.Const 10)) with
  | Error (`TypeError _) -> true
  | _ -> false
;;

let%test "eval step limit reached with loop" =
  let loop = Ast.LetRec ("loop", "x", Ast.App (Ast.Var "loop", Ast.Var "x"), Ast.App (Ast.Var "loop", Ast.Const 1)) in
  match eval ~step_limit:50 () loop with
  | Error `StepLimitReached -> true
  | _ -> false
;;

let%test "eval step limit with recursive sum" =
  let sum_body =
    Ast.If
      ( Ast.BinOp (Ast.Le, Ast.Var "n", Ast.Const 0)
      , Ast.Const 0
      , Ast.BinOp (Ast.Add, Ast.Var "n", Ast.App (Ast.Var "sum", Ast.BinOp (Ast.Sub, Ast.Var "n", Ast.Const 1))) )
  in
  match
    eval ~step_limit:10 () (Ast.LetRec ("sum", "n", sum_body, Ast.App (Ast.Var "sum", Ast.Const 100)))
  with
  | Error `StepLimitReached -> true
  | _ -> false
;;

let%test "eval with large step limit" =
  let fact_body =
    Ast.If
      ( Ast.BinOp (Ast.Gt, Ast.Var "n", Ast.Const 0)
      , Ast.BinOp (Ast.Mul, Ast.App (Ast.Var "fact", Ast.BinOp (Ast.Sub, Ast.Var "n", Ast.Const 1)), Ast.Var "n")
      , Ast.Const 1 )
  in
  match
    eval
      ~step_limit:10000
      ()
      (Ast.LetRec ("fact", "n", fact_body, Ast.App (Ast.Var "fact", Ast.Const 10)))
  with
  | Ok (VInt n) when n = 3628800 -> true
  | _ -> false
;;

let%test "eval curried three arguments" =
  let add3 =
    Ast.Abs ("x", Ast.Abs ("y", Ast.Abs ("z", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.BinOp (Ast.Add, Ast.Var "y", Ast.Var "z")))))
  in
  test_eval (Ast.App (Ast.App (Ast.App (add3, Ast.Const 1), Ast.Const 2), Ast.Const 3)) 6
;;

let%test "eval function returning function" =
  let maker = Ast.Abs ("x", Ast.Abs ("y", Ast.BinOp (Ast.Mul, Ast.Var "x", Ast.Var "y"))) in
  test_eval
    (Ast.Let
       ( "maker"
       , maker
       , Ast.Let ("mul5", Ast.App (Ast.Var "maker", Ast.Const 5), Ast.App (Ast.Var "mul5", Ast.Const 7)) ))
    35
;;

let%test "eval comparison chain" =
  test_eval (Ast.BinOp (Ast.Lt, Ast.BinOp (Ast.Add, Ast.Const 1, Ast.Const 2), Ast.BinOp (Ast.Mul, Ast.Const 2, Ast.Const 3))) 1
;;

let%test "eval min function" =
  let min_body = Ast.If (Ast.BinOp (Ast.Lt, Ast.Var "a", Ast.Var "b"), Ast.Var "a", Ast.Var "b") in
  test_eval
    (Ast.Let ("min", Ast.Abs ("a", Ast.Abs ("b", min_body)), Ast.App (Ast.App (Ast.Var "min", Ast.Const 5), Ast.Const 10)))
    5
;;

let%test "eval max function" =
  let max_body = Ast.If (Ast.BinOp (Ast.Gt, Ast.Var "a", Ast.Var "b"), Ast.Var "a", Ast.Var "b") in
  test_eval
    (Ast.Let ("max", Ast.Abs ("a", Ast.Abs ("b", max_body)), Ast.App (Ast.App (Ast.Var "max", Ast.Const 5), Ast.Const 10)))
    10
;;

let%test "eval abs function" =
  let abs_body =
    Ast.If (Ast.BinOp (Ast.Lt, Ast.Var "x", Ast.Const 0), Ast.BinOp (Ast.Sub, Ast.Const 0, Ast.Var "x"), Ast.Var "x")
  in
  test_eval (Ast.Let ("abs", Ast.Abs ("x", abs_body), Ast.App (Ast.Var "abs", Ast.Const (-5)))) 5
  && test_eval (Ast.Let ("abs", Ast.Abs ("x", abs_body), Ast.App (Ast.Var "abs", Ast.Const 5))) 5
;;
