[@@@ocaml.text "/*"]

(** Copyright 2026, [ChurinNick] *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib.Ast
open Lambda_lib.Parser

let run_parse input =
  match parse input with
  | Ok expr -> print_endline (show_expr expr)
  | Error (`Parsing_error msg) -> print_endline ("Parsing error: " ^ msg)
;;

let%expect_test "literals" =
  run_parse "42";
  run_parse "0";
  run_parse "-7";
  run_parse "()";
  run_parse "x";
  run_parse "fact_1";
  run_parse "(((x)))";
  run_parse "(1 + 2) = ((3 * 1) < (4 + 2)) > ((10 / 5) <> (2 * 2))";
  [%expect
    {|
    (Lit (Ast.Integer 42))
    (Lit (Ast.Integer 0))
    (UnOp (Ast.Negate, (Lit (Ast.Integer 7))))
    (Lit Ast.UnitVal)
    (Var "x")
    (Var "fact_1")
    (Var "x")
    Parsing error: Parsing error
  |}]
;;

let%expect_test "unary_minus" =
  run_parse "-x";
  run_parse "-(x + y)";
  run_parse "5 * -3";
  run_parse "-x + y";
  [%expect
    {|
    (UnOp (Ast.Negate, (Var "x")))
    (UnOp (Ast.Negate, (BinOp (Ast.Add, (Var "x"), (Var "y")))))
    (BinOp (Ast.Mul, (Lit (Ast.Integer 5)),
       (UnOp (Ast.Negate, (Lit (Ast.Integer 3))))))
    (BinOp (Ast.Add, (UnOp (Ast.Negate, (Var "x"))), (Var "y")))
  |}]
;;

let%expect_test "arithmetic_precedence" =
  run_parse "1 + 2 * 3";
  run_parse "(1 + 2) * 3";
  run_parse "3 - -5";
  run_parse "10 - 2 / 2";
  run_parse "a * b + c * d";
  run_parse "((4 + 6) * (1 - 9)) / 10";
  run_parse "1 + 2 * 3 - 4 / 2";
  [%expect
    {|
    (BinOp (Ast.Add, (Lit (Ast.Integer 1)),
       (BinOp (Ast.Mul, (Lit (Ast.Integer 2)), (Lit (Ast.Integer 3))))))
    (BinOp (Ast.Mul,
       (BinOp (Ast.Add, (Lit (Ast.Integer 1)), (Lit (Ast.Integer 2)))),
       (Lit (Ast.Integer 3))))
    (BinOp (Ast.Sub, (Lit (Ast.Integer 3)),
       (UnOp (Ast.Negate, (Lit (Ast.Integer 5))))))
    (BinOp (Ast.Sub, (Lit (Ast.Integer 10)),
       (BinOp (Ast.Div, (Lit (Ast.Integer 2)), (Lit (Ast.Integer 2))))))
    (BinOp (Ast.Add, (BinOp (Ast.Mul, (Var "a"), (Var "b"))),
       (BinOp (Ast.Mul, (Var "c"), (Var "d")))))
    (BinOp (Ast.Div,
       (BinOp (Ast.Mul,
          (BinOp (Ast.Add, (Lit (Ast.Integer 4)), (Lit (Ast.Integer 6)))),
          (BinOp (Ast.Sub, (Lit (Ast.Integer 1)), (Lit (Ast.Integer 9)))))),
       (Lit (Ast.Integer 10))))
    (BinOp (Ast.Sub,
       (BinOp (Ast.Add, (Lit (Ast.Integer 1)),
          (BinOp (Ast.Mul, (Lit (Ast.Integer 2)), (Lit (Ast.Integer 3)))))),
       (BinOp (Ast.Div, (Lit (Ast.Integer 4)), (Lit (Ast.Integer 2))))))
  |}]
;;

let%expect_test "left_associativity" =
  run_parse "1 - 2 - 3";
  run_parse "8 / 2 / 2";
  [%expect
    {|
    (BinOp (Ast.Sub,
       (BinOp (Ast.Sub, (Lit (Ast.Integer 1)), (Lit (Ast.Integer 2)))),
       (Lit (Ast.Integer 3))))
    (BinOp (Ast.Div,
       (BinOp (Ast.Div, (Lit (Ast.Integer 8)), (Lit (Ast.Integer 2)))),
       (Lit (Ast.Integer 2))))
  |}]
;;

let%expect_test "comparisons" =
  run_parse "x = y";
  run_parse "x >= y";
  run_parse "x <= y";
  run_parse "x <> y";
  run_parse "x = y";
  run_parse "x >= 1";
  run_parse "3 <= y";
  run_parse "1 <> 9";
  run_parse "-x < 0";
  [%expect
    {|
    (CmpOp (Ast.Eq, (Var "x"), (Var "y")))
    (CmpOp (Ast.Ge, (Var "x"), (Var "y")))
    (CmpOp (Ast.Le, (Var "x"), (Var "y")))
    (CmpOp (Ast.Neq, (Var "x"), (Var "y")))
    (CmpOp (Ast.Eq, (Var "x"), (Var "y")))
    (CmpOp (Ast.Ge, (Var "x"), (Lit (Ast.Integer 1))))
    (CmpOp (Ast.Le, (Lit (Ast.Integer 3)), (Var "y")))
    (CmpOp (Ast.Neq, (Lit (Ast.Integer 1)), (Lit (Ast.Integer 9))))
    (CmpOp (Ast.Lt, (UnOp (Ast.Negate, (Var "x"))), (Lit (Ast.Integer 0))))
  |}]
;;

let%expect_test "if_expressions" =
  run_parse "if n then 1 else 0";
  run_parse "if x > 0 then x else -x";
  run_parse "if x then if y then 1 else 2 else 3";
  run_parse "if x > 0 then x + 1 else x - 1";
  [%expect
    {|
    (If ((Var "n"), (Lit (Ast.Integer 1)), (Lit (Ast.Integer 0))))
    (If ((CmpOp (Ast.Gt, (Var "x"), (Lit (Ast.Integer 0)))), (Var "x"),
       (UnOp (Ast.Negate, (Var "x")))))
    (If ((Var "x"),
       (If ((Var "y"), (Lit (Ast.Integer 1)), (Lit (Ast.Integer 2)))),
       (Lit (Ast.Integer 3))))
    (If ((CmpOp (Ast.Gt, (Var "x"), (Lit (Ast.Integer 0)))),
       (BinOp (Ast.Add, (Var "x"), (Lit (Ast.Integer 1)))),
       (BinOp (Ast.Sub, (Var "x"), (Lit (Ast.Integer 1))))))
  |}]
;;

let%expect_test "lambdas" =
  run_parse "fun x -> x";
  run_parse "fun a b c -> a * b + c";
  [%expect
    {|
    (Lam ("x", (Var "x")))
    (Lam ("a",
       (Lam ("b",
          (Lam ("c",
             (BinOp (Ast.Add, (BinOp (Ast.Mul, (Var "a"), (Var "b"))), (Var "c")
                ))
             ))
          ))
       ))
  |}]
;;

let%expect_test "different_applications" =
  run_parse "f x";
  run_parse "f x y z";
  run_parse "f (x + 1) (y * 2)";
  run_parse "f x / y";
  run_parse " f x + g y";
  run_parse "f x = g((y+1)/(z-1))";
  run_parse "f (let x = 1 in x)";
  run_parse "f (if x then 1 else 2)";
  run_parse "if f x then 1 else 2";
  run_parse "f (let x = 1 in x) (if y then 3 else 4)";
  [%expect
    {|
    (App ((Var "f"), (Var "x")))
    (App ((App ((App ((Var "f"), (Var "x"))), (Var "y"))), (Var "z")))
    (App ((App ((Var "f"), (BinOp (Ast.Add, (Var "x"), (Lit (Ast.Integer 1)))))),
       (BinOp (Ast.Mul, (Var "y"), (Lit (Ast.Integer 2))))))
    (BinOp (Ast.Div, (App ((Var "f"), (Var "x"))), (Var "y")))
    (BinOp (Ast.Add, (App ((Var "f"), (Var "x"))), (App ((Var "g"), (Var "y")))))
    (CmpOp (Ast.Eq, (App ((Var "f"), (Var "x"))),
       (App ((Var "g"),
          (BinOp (Ast.Div, (BinOp (Ast.Add, (Var "y"), (Lit (Ast.Integer 1)))),
             (App ((Var "z"), (Lit (Ast.Integer -1))))))
          ))
       ))
    (App ((Var "f"), (Let (Ast.Plain, "x", (Lit (Ast.Integer 1)), (Var "x")))))
    (App ((Var "f"),
       (If ((Var "x"), (Lit (Ast.Integer 1)), (Lit (Ast.Integer 2))))))
    (If ((App ((Var "f"), (Var "x"))), (Lit (Ast.Integer 1)),
       (Lit (Ast.Integer 2))))
    (App (
       (App ((Var "f"), (Let (Ast.Plain, "x", (Lit (Ast.Integer 1)), (Var "x")))
          )),
       (If ((Var "y"), (Lit (Ast.Integer 3)), (Lit (Ast.Integer 4))))))
  |}]
;;

let%expect_test "let_bindings" =
  run_parse "let x = 5 in x + 1";
  run_parse "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5";
  run_parse "let x = if y then 1 else 2 in x + 3";
  [%expect
    {|
    (Let (Ast.Plain, "x", (Lit (Ast.Integer 5)),
       (BinOp (Ast.Add, (Var "x"), (Lit (Ast.Integer 1))))))
    (Let (Ast.Recursive, "fact",
       (Lam ("n",
          (If ((CmpOp (Ast.Le, (Var "n"), (Lit (Ast.Integer 1)))),
             (Lit (Ast.Integer 1)),
             (BinOp (Ast.Mul, (Var "n"),
                (App ((Var "fact"),
                   (BinOp (Ast.Sub, (Var "n"), (Lit (Ast.Integer 1))))))
                ))
             ))
          )),
       (App ((Var "fact"), (Lit (Ast.Integer 5))))))
    (Let (Ast.Plain, "x",
       (If ((Var "y"), (Lit (Ast.Integer 1)), (Lit (Ast.Integer 2)))),
       (BinOp (Ast.Add, (Var "x"), (Lit (Ast.Integer 3))))))
  |}]
;;

let%expect_test "fix_combinator" =
  run_parse "fix f";
  run_parse "fix (fun f -> fun n -> if n then n * f (n - 1) else 1)";
  run_parse "fix (fun f -> fun x -> f x)";
  [%expect
    {|
    (Fix (Var "f"))
    (Fix
       (Lam ("f",
          (Lam ("n",
             (If ((Var "n"),
                (BinOp (Ast.Mul, (Var "n"),
                   (App ((Var "f"),
                      (BinOp (Ast.Sub, (Var "n"), (Lit (Ast.Integer 1))))))
                   )),
                (Lit (Ast.Integer 1))))
             ))
          )))
    (Fix (Lam ("f", (Lam ("x", (App ((Var "f"), (Var "x"))))))))
  |}]
;;

let%expect_test "complex_nested" =
  run_parse "let f = fun x -> x * x in if f 5 > 20 then f (2 + 3) else 0";
  run_parse
    "let rec fact_helper = fun acc n -> if n = 0 then acc else fact_helper (acc * n) (n \
     - 1) in let fact = fun n -> fact_helper 1 n in fact 5";
  run_parse
    "let rec fibonacci = fun n -> if n <= 1 then n else fibonacci (n - 1) + fibonacci (n \
     - 2) in fibonacci 1";
  [%expect
    {|
    (Let (Ast.Plain, "f", (Lam ("x", (BinOp (Ast.Mul, (Var "x"), (Var "x"))))),
       (If (
          (CmpOp (Ast.Gt, (App ((Var "f"), (Lit (Ast.Integer 5)))),
             (Lit (Ast.Integer 20)))),
          (App ((Var "f"),
             (BinOp (Ast.Add, (Lit (Ast.Integer 2)), (Lit (Ast.Integer 3)))))),
          (Lit (Ast.Integer 0))))
       ))
    (Let (Ast.Recursive, "fact_helper",
       (Lam ("acc",
          (Lam ("n",
             (If ((CmpOp (Ast.Eq, (Var "n"), (Lit (Ast.Integer 0)))),
                (Var "acc"),
                (App (
                   (App ((Var "fact_helper"),
                      (BinOp (Ast.Mul, (Var "acc"), (Var "n"))))),
                   (BinOp (Ast.Sub, (Var "n"), (Lit (Ast.Integer 1))))))
                ))
             ))
          )),
       (Let (Ast.Plain, "fact",
          (Lam ("n",
             (App ((App ((Var "fact_helper"), (Lit (Ast.Integer 1)))), (Var "n")
                ))
             )),
          (App ((Var "fact"), (Lit (Ast.Integer 5))))))
       ))
    (Let (Ast.Recursive, "fibonacci",
       (Lam ("n",
          (If ((CmpOp (Ast.Le, (Var "n"), (Lit (Ast.Integer 1)))), (Var "n"),
             (BinOp (Ast.Add,
                (App ((Var "fibonacci"),
                   (BinOp (Ast.Sub, (Var "n"), (Lit (Ast.Integer 1)))))),
                (App ((Var "fibonacci"),
                   (BinOp (Ast.Sub, (Var "n"), (Lit (Ast.Integer 2))))))
                ))
             ))
          )),
       (App ((Var "fibonacci"), (Lit (Ast.Integer 1))))))
  |}]
;;

let%expect_test "parse_errors" =
  run_parse "let";
  run_parse "1 +";
  run_parse "(1 + 2";
  run_parse "let rec";
  run_parse "let x=";
  run_parse "";
  [%expect
    {|
    Parsing error: Parsing error
    Parsing error: Parsing error
    Parsing error: Parsing error
    Parsing error: Parsing error
    Parsing error: Parsing error
    Parsing error: Parsing error
  |}]
;;
