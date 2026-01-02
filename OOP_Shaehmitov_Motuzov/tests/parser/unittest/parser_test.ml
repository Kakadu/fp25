[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser
open Ast

let run_test input =
  match parse input with
  | Ok expr -> Stdlib.print_endline (show_expr expr)
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

let run_program_parser input =
  match parse_structure_items input with
  | Ok expr -> Stdlib.print_endline (show_program expr)
  | Error msg -> Stdlib.Printf.printf "Error: %s\n" msg
;;

let%expect_test "simple structure" =
  run_program_parser "let x = 10 + 5 - 6";
  [%expect
    {|
    [(Value
        (NonRec, (PVar "x"),
         (BinOp (Sub, (BinOp (Add, (Const (CInt 10)), (Const (CInt 5)))),
            (Const (CInt 6))))))
      ] |}]
;;

let%expect_test "binding test" =
  run_program_parser "let f x y = x + y";
  [%expect
    {|
   [(Value
       (NonRec, (PVar "f"),
        (FunExpr ([(PVar "x"); (PVar "y")], (BinOp (Add, (Var "x"), (Var "y")))
           ))))
     ] |}]
;;

let%expect_test "two binding" =
  run_program_parser "let f x y = x * y ;;\n  let g n = f n 5";
  [%expect
    {|
  [(Value
      (NonRec, (PVar "f"),
       (FunExpr ([(PVar "x"); (PVar "y")], (BinOp (Mul, (Var "x"), (Var "y")))
          ))));
    (Value
       (NonRec, (PVar "g"),
        (FunExpr ([(PVar "n")],
           (App ((App ((Var "f"), (Var "n"))), (Const (CInt 5))))))))
    ] |}]
;;

let%expect_test "binding and expression" =
  run_program_parser "let () = let x = 10 in print_int (x * 2)";
  [%expect
    {|
  [(Value
      (NonRec, PUnit,
       (Let (NonRec, (PVar "x"), (Const (CInt 10)),
          (App ((Var "print_int"), (BinOp (Mul, (Var "x"), (Const (CInt 2))))))
          ))))
    ] |}]
;;

let%expect_test "few let in" =
  run_program_parser "let f = \n      let x = 6 in\n      let y = 7 in\n      x + y ;;";
  [%expect
    {|
      [(Value
          (NonRec, (PVar "f"),
           (Let (NonRec, (PVar "x"), (Const (CInt 6)),
              (Let (NonRec, (PVar "y"), (Const (CInt 7)),
                 (BinOp (Add, (Var "x"), (Var "y")))))
              ))))
        ]
     |}]
;;

let%expect_test "lambda binding" =
  run_program_parser "let add = fun x y -> x + y ;; let x = add 3 4";
  [%expect
    {|
    [(Value
        (NonRec, (PVar "add"),
         (FunExpr ([(PVar "x"); (PVar "y")], (BinOp (Add, (Var "x"), (Var "y")))
            ))));
      (Value
         (NonRec, (PVar "x"),
          (App ((App ((Var "add"), (Const (CInt 3)))), (Const (CInt 4))))))
      ] |}]
;;

let%expect_test "unit function test" =
  run_program_parser "let () = print_int 42";
  [%expect
    {|
    [(Value (NonRec, PUnit, (App ((Var "print_int"), (Const (CInt 42))))))] |}]
;;

let%expect_test "unit pattern in let" =
  run_program_parser "let () = print_int 42 ";
  [%expect
    {|
    [(Value (NonRec, PUnit, (App ((Var "print_int"), (Const (CInt 42))))))]
    |}]
;;

let%expect_test "unit pattern in let with body" =
  run_program_parser
    "let printer a b = \n    let () = print_int a in \n    print_int b \n    ;; ";
  [%expect
    {|
    [(Value
        (NonRec, (PVar "printer"),
         (FunExpr ([(PVar "a"); (PVar "b")],
            (Let (NonRec, PUnit, (App ((Var "print_int"), (Var "a"))),
               (App ((Var "print_int"), (Var "b")))))
            ))))
      ] |}]
;;

let%expect_test "let in in let in" =
  run_program_parser "let f = let x = let y = 10 in y in x ;;";
  [%expect
    {|
    [(Value
        (NonRec, (PVar "f"),
         (Let (NonRec, (PVar "x"),
            (Let (NonRec, (PVar "y"), (Const (CInt 10)), (Var "y"))), (Var "x")))))
      ] |}]
;;

let%expect_test "application bin op" =
  run_program_parser "let f x = x + 1 ;; let y = f 10 + f 20;;";
  [%expect
    {|
    [(Value
        (NonRec, (PVar "f"),
         (FunExpr ([(PVar "x")], (BinOp (Add, (Var "x"), (Const (CInt 1))))))));
      (Value
         (NonRec, (PVar "y"),
          (BinOp (Add, (App ((Var "f"), (Const (CInt 10)))),
             (App ((Var "f"), (Const (CInt 20))))))))
      ] |}]
;;

let%expect_test "test1" =
  run_program_parser "letrec f = 1";
  [%expect {| Error: : end_of_input |}]
;;

let%expect_test "test1" =
  run_program_parser "let rec f __= 1";
  [%expect {| Error: : end_of_input |}]
;;

let%expect_test "test2" =
  run_program_parser "let rec f = if(n) then 1 else f(n-1)";
  [%expect
    {|
    [(Value
        (Rec, (PVar "f"),
         (If ((Var "n"), (Const (CInt 1)),
            (App ((Var "f"), (BinOp (Sub, (Var "n"), (Const (CInt 1))))))))))
      ] |}]
;;
