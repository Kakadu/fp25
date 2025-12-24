[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let print_result result =
  match result with
  | Ok v -> Printf.printf "Ok %s\n" (Interpret.string_of_value v)
  | Error err -> Printf.printf "Error %s\n" (Interpret.string_of_error err)
;;

let print_expr_result expr = print_result (Interpret.eval_expr expr)

let print_program program =
  match Interpret.eval_program program with
  | Ok values -> List.iter (fun v -> print_endline (Interpret.string_of_value v)) values
  | Error err -> Printf.printf "Error %s\n" (Interpret.string_of_error err)
;;

let%expect_test "eval_expr ok branches" =
  let open Ast in
  print_expr_result (Const (Bool true));
  print_expr_result (Const (Bool false));
  print_expr_result (Const (Unit ()));
  print_expr_result (Unop (UMinus, Const (Int 5)));
  print_expr_result (Unop (UPlus, Const (Int 5)));
  print_expr_result (Unop (Not, Const (Bool false)));
  print_expr_result (BinopArithmetic (Add, Const (Int 1), Const (Int 2)));
  print_expr_result (BinopArithmetic (Mul, Const (Int 2), Const (Int 3)));
  print_expr_result (BinopArithmetic (Div, Const (Int 4), Const (Int 2)));
  print_expr_result (BinopComp (Eq, Const (Int 1), Const (Int 1)));
  print_expr_result (BinopComp (Neq, Const (Bool true), Const (Bool false)));
  print_expr_result (BinopComp (Lt, Const (Int 1), Const (Int 2)));
  print_expr_result (BinopComp (Ge, Const (Int 2), Const (Int 2)));
  print_expr_result (BinopBool (And, Const (Bool true), Const (Bool false)));
  print_expr_result
    (BinopBool (And, Const (Bool false), Unop (UMinus, Const (Bool true))));
  print_expr_result (BinopBool (Or, Const (Bool true), Unop (UMinus, Const (Bool true))));
  print_expr_result (BinopBool (Or, Const (Bool false), Const (Bool true)));
  print_expr_result (Let (Nonrec, "x", Const (Int 1), Var "x"));
  print_expr_result (If (Const (Bool false), Const (Int 1), None));
  print_expr_result (If (Const (Int 0), Const (Int 1), Some (Const (Int 2))));
  print_expr_result (App (Lam ("x", Var "x"), Const (Int 7)));
  print_expr_result (Lam ("x", Var "x"));
  print_expr_result (Let (Rec, "id", Lam ("x", Var "x"), App (Var "id", Const (Int 3))));
  print_expr_result (Fix (Lam ("f", Lam ("x", Var "x"))));
  [%expect
    {|
    Ok true
    Ok false
    Ok ()
    Ok -5
    Ok 5
    Ok true
    Ok 3
    Ok 6
    Ok 2
    Ok true
    Ok true
    Ok true
    Ok true
    Ok false
    Ok false
    Ok true
    Ok true
    Ok 1
    Ok ()
    Ok 2
    Ok 7
    Ok <fun>
    Ok 3
    Ok <fun>
  |}]
;;

let%expect_test "eval_expr error branches" =
  let open Ast in
  print_expr_result (Var "missing");
  print_expr_result (Unop (UMinus, Const (Bool true)));
  print_expr_result (Unop (Not, Const (Unit ())));
  print_expr_result (App (Const (Int 1), Const (Int 2)));
  print_expr_result (BinopArithmetic (Div, Const (Int 1), Const (Int 0)));
  print_expr_result (Let (Rec, "f", Const (Int 1), Var "f"));
  print_expr_result (Fix (Const (Int 1)));
  print_expr_result (Fix (Lam ("f", Const (Int 1))));
  print_expr_result (BinopComp (Eq, Const (Int 1), Const (Bool true)));
  print_expr_result (If (Const (Unit ()), Const (Int 1), None));
  print_result (Interpret.eval_expr ~fuel:0 (Const (Int 1)));
  [%expect
    {|
    Error unbound variable: missing
    Error expected int, got true
    Error expected bool or int, got ()
    Error expected function, got 1
    Error division by zero
    Error let rec expects a function for f
    Error fix expects a function, got 1
    Error fix expects a function, got <fun>
    Error invalid comparison between 1 and true
    Error expected bool or int, got ()
    Error step limit exceeded
  |}]
;;

let%expect_test "eval_program branches" =
  let open Ast in
  print_program [ TLet (Nonrec, "x", Const (Int 1)); TExpr (Var "x") ];
  print_program
    [ TLet (Rec, "id", Lam ("x", Var "x")); TExpr (App (Var "id", Const (Int 2))) ];
  print_program [ TLet (Rec, "oops", Const (Int 1)) ];
  [%expect {|
    1
    2
    Error let rec expects a function for oops
  |}]
;;

let%expect_test "pprintast bool ops" =
  let open Ast in
  let exprs =
    [ Const (Bool true)
    ; Const (Bool false)
    ; Unop (Not, Var "x")
    ; BinopBool (And, Var "a", Var "b")
    ; BinopBool (Or, Var "a", Var "b")
    ]
  in
  List.iter (fun expr -> print_endline (Pprintast.string_of_expr expr)) exprs;
  [%expect {|
    true
    false
    (not x)
    (a && b)
    (a || b)
  |}]
;;

let%expect_test "ast show and eq for bool ops" =
  let open Ast in
  let expr_not = Unop (Not, Var "x") in
  let expr_or = BinopBool (Or, Var "a", Var "b") in
  print_endline (Ast.show_expr expr_not);
  print_endline (Ast.show_expr expr_or);
  print_endline (string_of_bool (Ast.equal_expr expr_not expr_not));
  print_endline (string_of_bool (Ast.equal_expr expr_or expr_or));
  [%expect
    {|
    (Unop (Not, (Var "x")))
    (BinopBool (Or, (Var "a"), (Var "b")))
    true
    true
  |}]
;;
