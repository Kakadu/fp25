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
  [%expect {| (Int 42) |}]
;;

let%expect_test "simple booleans" =
  run_test "true";
  [%expect {| (Bool true) |}]
;;

let%expect_test "variables" =
  run_test "x";
  [%expect {| (Var "x") |}]
;;

let%expect_test "simple addition" =
  run_test "1 + 2 ";
  [%expect {| (BinOp (Add, (Int 1), (Int 2))) |}]
;;

let%expect_test "operator precedence" =
  run_test "1 + 2 * 3";
  [%expect {| (BinOp (Add, (Int 1), (BinOp (Mul, (Int 2), (Int 3))))) |}]
;;

let%expect_test "parentheses" =
  run_test "(1 + 2) * 3";
  [%expect {| (BinOp (Mul, (BinOp (Add, (Int 1), (Int 2))), (Int 3))) |}]
;;

let%expect_test "unary operators" =
  run_test "not true";
  [%expect {| (UnOp (Not, (Bool true))) |}]
;;

let%expect_test "lambda function" =
  run_test "fun x -> x + 1";
  [%expect {| (Fun ("x", (BinOp (Add, (Var "x"), (Int 1))))) |}]
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
  [%expect {| (Let ("x", (Int 5), (BinOp (Add, (Var "x"), (Int 1))))) |}]
;;

let%expect_test "let with function" =
  run_test "let f x = x * 2 in f 3";
  [%expect
    {|
      (Let ("f", (Fun ("x", (BinOp (Mul, (Var "x"), (Int 2))))),
         (App ((Var "f"), (Int 3))))) |}]
;;

let%expect_test "if expression" =
  run_test "if true then 1 else 2";
  [%expect {| (If ((Bool true), (Int 1), (Int 2))) |}]
;;

let%expect_test "comparison operators" =
  run_test "1 < 2";
  [%expect {| (BinOp (Lt, (Int 1), (Int 2))) |}]
;;

let%expect_test "logical operators" =
  run_test "true && false";
  [%expect {| (BinOp (And, (Bool true), (Bool false))) |}]
;;

let%expect_test "unary minus" =
  run_test "-42";
  [%expect {| (UnOp (Neg, (Int 42))) |}]
;;

let%expect_test "unary minus inside expression" =
  run_test "1 + -2";
  [%expect {| (BinOp (Add, (Int 1), (UnOp (Neg, (Int 2))))) |}]
;;

let%expect_test "unary minus in parentheses" =
  run_test "(-42) + 3";
  [%expect {| (BinOp (Add, (UnOp (Neg, (Int 42))), (Int 3))) |}]
;;

let%expect_test "not operator with expression" =
  run_test "not (1 < 2)";
  [%expect {| (UnOp (Not, (BinOp (Lt, (Int 1), (Int 2))))) |}]
;;

let%expect_test "nested subtraction" =
  run_test "19 - (10 - 6)";
  [%expect {| (BinOp (Sub, (Int 19), (BinOp (Sub, (Int 10), (Int 6))))) |}]
;;

let%expect_test "nested subtraction with unary minus" =
  run_test "19 - (10 - -6)";
  [%expect {| (BinOp (Sub, (Int 19), (BinOp (Sub, (Int 10), (UnOp (Neg, (Int 6))))))) |}]
;;

let%expect_test "factorial function" =
  run_test "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5";
  [%expect
    {|
    (LetRec ("fact",
       (Fun ("n",
          (If ((BinOp (Eq, (Var "n"), (Int 0))), (Int 1),
             (BinOp (Mul, (Var "n"),
                (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Int 1)))))))
             ))
          )),
       (App ((Var "fact"), (Int 5))))) |}]
;;

let%expect_test "two let" =
  run_test "let let x = 5";
  [%expect {| Error: : no more choices |}]
;;

let%expect_test "application1" =
  run_test "(f x) y";
  [%expect {| (App ((App ((Var "f"), (Var "x"))), (Var "y"))) |}]
;;

let%expect_test "application1" =
  run_test "((f || 0) 1)";
  [%expect {| (App ((BinOp (Or, (Var "f"), (Int 0))), (Int 1))) |}]
;;

let%expect_test "application2" =
  run_test "f (x y)";
  [%expect {| (App ((Var "f"), (App ((Var "x"), (Var "y"))))) |}]
;;
