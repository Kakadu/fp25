(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Filichkin_lib.Parser
open Filichkin_lib.Print

let%expect_test "parse toplevel sequence" =
  print_ast_p (parser "let x = 1;; let y = 2;; x + y" |> Result.get_ok);
  [%expect
    {|
    Let (NonRec, "Var x", Int 1, None)
    Let (NonRec, "Var y", Int 2, None)
    BinOp (Plus, Var "x", Var "y")
|}]
;;

let%expect_test "parse match tuple pattern" =
  print_ast_p (parser "match (1, 2) with | (x, y) -> x + y" |> Result.get_ok);
  [%expect
    {| Match (Tuple [Int 1; Int 2], [(PTuple [Var x; Var y], BinOp (Plus, Var "x", Var "y"))]) |}]
;;

let%expect_test "parse match without leading bar" =
  print_ast_p (parser "match (1, 2) with (x, y) -> x + y | _ -> 0" |> Result.get_ok);
  [%expect
    {|
    Match (Tuple [Int 1; Int 2], [(PTuple [Var x; Var y], BinOp (Plus, Var "x", Var "y")); (_, Int 0)])
|}]
;;

let%expect_test "parse fun with tuple pattern" =
  print_ast_p (parser "fun (x, y) -> x + y" |> Result.get_ok);
  [%expect {| Abs ("(x, y)", BinOp (Plus, Var "x", Var "y")) |}]
;;
