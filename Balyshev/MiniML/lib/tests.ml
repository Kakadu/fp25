open MiniML
module Eval = Interpreter.Eval (Monads.RESULT_MONAD)

let run_expr str =
  match Parser.parse_expression str with
  | Ok ast -> Ast.show_expression ast |> Format.printf "parsed: %s"
  | Error err -> err |> print_endline
;;

let%expect_test _ =
  run_expr "123";
  [%expect {| parsed: 123 |}]
;;

let%expect_test _ =
  run_expr "1 + 2 + 3";
  [%expect
    {|
    parsed: ((1 + 2) + 3) |}]
;;

let%expect_test _ =
  run_expr "1 + 2 * 3";
  [%expect
    {|
    parsed: (1 + (2 * 3)) |}]
;;

let%expect_test _ =
  run_expr "(1, 2, 3)";
  [%expect {| parsed: (1, 2, 3) |}]
;;

let%expect_test _ =
  run_expr "1, 2, 3";
  [%expect {| parsed: (1, 2, 3) |}]
;;

let%expect_test _ =
  run_expr "1 + 2, 3 + 4";
  [%expect
    {|
    parsed: ((1 + 2), (3 + 4)) |}]
;;

let%expect_test _ =
  run_expr "(1 + 2, 3 + 4)";
  [%expect
    {|
    parsed: ((1 + 2), (3 + 4)) |}]
;;

let%expect_test _ =
  run_expr "a :: b :: c :: d";
  [%expect
    {|
    parsed: (a :: (b :: (c :: d))) |}]
;;

let%expect_test _ =
  run_expr "a :: b = c :: d";
  [%expect
    {|
    parsed: ((a :: b) = (c :: d)) |}]
;;

let%expect_test _ =
  run_expr "a * b >= 1 + 2 / 3";
  [%expect
    {|
    parsed: ((a * b) >= (1 + (2 / 3))) |}]
;;

(* *)

let parse_eval_print expr =
  match Parser.parse_expression expr with
  | Error err -> Printf.printf "parsing failed : %s" err
  | Ok ast ->
    (match Eval.eval_expr ast with
     | Ok value -> Printf.printf "evaluated : %s" (Interpreter.show_value value)
     | Error err -> Printf.printf "interpreting failed : %s" (Interpreter.show_error err))
;;

let%expect_test _ =
  parse_eval_print "1 + 2 + 3";
  [%expect {| evaluated : 6 |}]
;;

let%expect_test _ =
  parse_eval_print "(1 + 2)";
  [%expect {| evaluated : 3 |}]
;;

let%expect_test _ =
  parse_eval_print "(1 + 2, 2, 3 - 2)";
  [%expect {| evaluated : (3, 2, 1) |}]
;;

(* *)

let eval_ast ast =
  match Eval.eval_expr ast with
  | Ok value -> Printf.printf "evaluated : %s" (Interpreter.show_value value)
  | Error err -> Printf.printf "interpreting failed : %s" (Interpreter.show_error err)
;;

open Ast

let%expect_test _ =
  let ast = EApp (EFun (PVar "x", EVar "x"), EFun (PVar "x", EVar "x")) in
  eval_ast ast;
  [%expect {| evaluated : (x -> x) |}]
;;

let%expect_test _ =
  let _text = "(fun x -> x + 3) (1 + 2)" in
  let ast =
    EApp
      ( EFun (PVar "x", EBinop (Add, EVar "x", EConstant (CInt 3)))
      , EBinop (Add, EConstant (CInt 1), EConstant (CInt 2)) )
  in
  eval_ast ast;
  [%expect {| evaluated : 6 |}]
;;

let%expect_test _ =
  let _text = "(fun f -> fun x -> f x) (fun y -> y + 1) 1" in
  let ast =
    EApp
      ( EApp
          ( EFun (PVar "f", EFun (PVar "x", EApp (EVar "f", EVar "x")))
          , EFun (PVar "y", EBinop (Add, EVar "y", EConstant (CInt 1))) )
      , EConstant (CInt 1) )
  in
  eval_ast ast;
  [%expect {| evaluated : 2 |}]
;;

let%expect_test _ =
  let _text = "(fun f -> fun x -> f x) (fun y -> y + 1) 1" in
  let ast =
    EApp
      ( EApp
          ( EFun (PVar "f", EFun (PVar "x", EApp (EVar "f", EVar "x")))
          , EFun (PVar "y", EBinop (Add, EVar "y", EConstant (CInt 1))) )
      , EConstant (CInt 1) )
  in
  eval_ast ast;
  [%expect {| evaluated : 2 |}]
;;

let%expect_test _ =
  let _text = "" in
  let ast =
    EApp
      ( EApp
          ( EFun (PVar "f", EFun (PVar "x", EApp (EVar "f", EVar "x")))
          , EFun (PVar "y", EBinop (Add, EVar "y", EConstant (CInt 1))) )
      , EConstant (CInt 1) )
  in
  eval_ast ast;
  [%expect {| evaluated : 2 |}]
;;
