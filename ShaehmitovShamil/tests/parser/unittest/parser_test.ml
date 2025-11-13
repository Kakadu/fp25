open Base
open Parser
open Ast

let run_test input =
  match parse input with
  | Ok expr -> print_endline (show_expr expr)
  | Error msg -> Stdlib.Printf.printf "Error: %s\n" msg
;;

let%expect_test "simple integers" =
  run_test "42";
  [%expect {| (Const (CInt 42)) |}]
;;

let%expect_test "variables" =
  run_test "x";
  [%expect {| (Var "x") |}]
;;

let%expect_test "simple addition" =
  run_test "1 + 2 ";
  [%expect {| (BinOp (Add, (Const (CInt 1)), (Const (CInt 2)))) |}]
;;

let%expect_test "operator precedence" =
  run_test "1 + 2 * 3";
  [%expect
    {|
    (BinOp (Add, (Const (CInt 1)),
       (BinOp (Mul, (Const (CInt 2)), (Const (CInt 3)))))) |}]
;;

let%expect_test "parentheses" =
  run_test "(1 + 2) * 3";
  [%expect
    {|
    (BinOp (Mul, (BinOp (Add, (Const (CInt 1)), (Const (CInt 2)))),
       (Const (CInt 3)))) |}]
;;

let%expect_test "lambda function" =
  run_test "fun x -> x + 1";
  [%expect {| (FunExpr ([(PVar "x")], (BinOp (Add, (Var "x"), (Const (CInt 1)))))) |}]
;;

let%expect_test "function application" =
  run_test "f x";
  [%expect {| (App ((Var "f"), (Var "x"))) |}]
;;

let%expect_test "multiple function application" =
  run_test "f x y";
  [%expect {| (App ((App ((Var "f"), (Var "x"))), (Var "y"))) |}]
;;

let%expect_test "let expression" =
  run_test "let x = 5 in x + 1";
  [%expect
    {|
    (Let (NonRec, (PVar "x"), (Const (CInt 5)),
       (BinOp (Add, (Var "x"), (Const (CInt 1)))))) |}]
;;

let%expect_test "let with function" =
  run_test "let f x = x * 2 in f 3";
  [%expect
    {|
      (Let (NonRec, (PVar "f"),
         (FunExpr ([(PVar "x")], (BinOp (Mul, (Var "x"), (Const (CInt 2)))))),
         (App ((Var "f"), (Const (CInt 3)))))) |}]
;;

let%expect_test "unary minus" =
  run_test "-42";
  [%expect {| (UnOp (Neg, (Const (CInt 42)))) |}]
;;

let%expect_test "unary minus inside expression" =
  run_test "1 + -2";
  [%expect {| (BinOp (Add, (Const (CInt 1)), (UnOp (Neg, (Const (CInt 2)))))) |}]
;;

let%expect_test "unary minus in parentheses" =
  run_test "(-42) + 3";
  [%expect {| (BinOp (Add, (UnOp (Neg, (Const (CInt 42)))), (Const (CInt 3)))) |}]
;;

let%expect_test "nested subtraction" =
  run_test "19 - (10 - 6)";
  [%expect
    {|
    (BinOp (Sub, (Const (CInt 19)),
       (BinOp (Sub, (Const (CInt 10)), (Const (CInt 6)))))) |}]
;;

let%expect_test "nested subtraction with unary minus" =
  run_test "19 - (10 - -6)";
  [%expect
    {|
    (BinOp (Sub, (Const (CInt 19)),
       (BinOp (Sub, (Const (CInt 10)), (UnOp (Neg, (Const (CInt 6)))))))) |}]
;;

let%expect_test "factorial function" =
  run_test "let rec fact n = if n then 1 else n * fact (n - 1) in fact 5";
  [%expect
    {|
    (Let (Rec, (PVar "fact"),
       (FunExpr ([(PVar "n")],
          (If ((Var "n"), (Const (CInt 1)),
             (BinOp (Mul, (Var "n"),
                (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Const (CInt 1))))))
                ))
             ))
          )),
       (App ((Var "fact"), (Const (CInt 5)))))) |}]
;;

let%expect_test "two let" =
  run_test "let let x = 5 in x + 1";
  [%expect {| Error: : no more choices |}]
;;

let%expect_test "application1" =
  run_test "(f x) y";
  [%expect {| (App ((App ((Var "f"), (Var "x"))), (Var "y"))) |}]
;;

let%expect_test "application2" =
  run_test "f (x y)";
  [%expect {| (App ((Var "f"), (App ((Var "x"), (Var "y"))))) |}]
;;

let%expect_test "fun with two arguments" =
  run_test "fun x y -> x + y";
  [%expect
    {|
    (FunExpr ([(PVar "x"); (PVar "y")], (BinOp (Add, (Var "x"), (Var "y"))))) |}]
;;
