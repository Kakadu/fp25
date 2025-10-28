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

let%expect_test "simple booleans" =
  run_test "true";
  [%expect {| (Const (CBool true)) |}]
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
  [%expect {|
    (BinOp (Add, (Const (CInt 1)),
       (BinOp (Mul, (Const (CInt 2)), (Const (CInt 3)))))) |}]
;;

let%expect_test "parentheses" =
  run_test "(1 + 2) * 3";
  [%expect {|
    (BinOp (Mul, (BinOp (Add, (Const (CInt 1)), (Const (CInt 2)))),
       (Const (CInt 3)))) |}]
;;

let%expect_test "unary operators" =
  run_test "not true";
  [%expect {| (UnOp (Not, (Const (CBool true)))) |}]
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
  [%expect {|
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

let%expect_test "if expression" =
  run_test "if true then 1 else 2";
  [%expect {| (If ((Const (CBool true)), (Const (CInt 1)), (Const (CInt 2)))) |}]
;;

let%expect_test "comparison operators" =
  run_test "1 < 2";
  [%expect {| (BinOp (Lt, (Const (CInt 1)), (Const (CInt 2)))) |}]
;;

let%expect_test "logical operators" =
  run_test "true && false";
  [%expect {| (BinOp (And, (Const (CBool true)), (Const (CBool false)))) |}]
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

let%expect_test "not operator with expression" =
  run_test "not (1 < 2)";
  [%expect {| (UnOp (Not, (BinOp (Lt, (Const (CInt 1)), (Const (CInt 2)))))) |}]
;;

let%expect_test "nested subtraction" =
  run_test "19 - (10 - 6)";
  [%expect {|
    (BinOp (Sub, (Const (CInt 19)),
       (BinOp (Sub, (Const (CInt 10)), (Const (CInt 6)))))) |}]
;;

let%expect_test "nested subtraction with unary minus" =
  run_test "19 - (10 - -6)";
  [%expect {|
    (BinOp (Sub, (Const (CInt 19)),
       (BinOp (Sub, (Const (CInt 10)), (UnOp (Neg, (Const (CInt 6)))))))) |}]
;;

let%expect_test "factorial function" =
  run_test "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5";
  [%expect
    {|
    (Let (Rec, (PVar "fact"),
       (FunExpr ([(PVar "n")],
          (If ((BinOp (Eq, (Var "n"), (Const (CInt 0)))), (Const (CInt 1)),
             (BinOp (Mul, (Var "n"),
                (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Const (CInt 1))))))
                ))
             ))
          )),
       (App ((Var "fact"), (Const (CInt 5)))))) |}]
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
  [%expect {| (App ((BinOp (Or, (Var "f"), (Const (CInt 0)))), (Const (CInt 1)))) |}]
;;

let%expect_test "application2" =
  run_test "f (x y)";
  [%expect {| (App ((Var "f"), (App ((Var "x"), (Var "y"))))) |}]
;;

let%expect_test "tuple expression" =
  run_test "(1, 2)";
  [%expect {| (Tuple [(Const (CInt 1)); (Const (CInt 2))]) |}]
;;

let%expect_test "tuple with variables" =
  run_test "(x, y, z)";
  [%expect {| (Tuple [(Var "x"); (Var "y"); (Var "z")]) |}]
;;

let%expect_test "nested tuple" =
  run_test "((1, 2), 3)";
  [%expect {| (Tuple [(Tuple [(Const (CInt 1)); (Const (CInt 2))]); (Const (CInt 3))]) |}]
;;

let%expect_test "match expression" =
  run_test "match x with y -> 1 | _ -> 0";
  [%expect {|
    (Match ((Var "x"), [((PVar "y"), (Const (CInt 1))); (PAny, (Const (CInt 0)))]
       )) |}]
;;

let%expect_test "match with tuple" =
  run_test "match (1, 2) with (x, y) -> x + y";
  [%expect {|
    (Match ((Tuple [(Const (CInt 1)); (Const (CInt 2))]),
       [((PTuple [(PVar "x"); (PVar "y")]), (BinOp (Add, (Var "x"), (Var "y"))))]
       )) |}]
;;

let%expect_test "let with tuple pattern" =
  run_test "let (x, y) = (1, 2) in x + y";
  [%expect {|
    (Let (NonRec, (PTuple [(PVar "x"); (PVar "y")]),
       (Tuple [(Const (CInt 1)); (Const (CInt 2))]),
       (BinOp (Add, (Var "x"), (Var "y"))))) |}]
;;

let%expect_test "fun with tuple pattern" =
  run_test "fun (x, y) -> x + y";
  [%expect {|
    (FunExpr ([(PTuple [(PVar "x"); (PVar "y")])],
       (BinOp (Add, (Var "x"), (Var "y"))))) |}]
;;
