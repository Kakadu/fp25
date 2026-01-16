(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Filichkin_lib.Parser
open Filichkin_lib.Print

let%expect_test "parse adt type declaration" =
  print_ast_p
    (parser
       {|type 'a mylist = Nil | Cons of 'a * 'a mylist|}
     |> Result.get_ok);
  [%expect
    {|
    [type_name mylist, type_params [a], constructors [{ ctor_name = Nil; ctor_args = [] }; { ctor_name = Cons; ctor_args = [TEVar a; TEConstr (mylist, [TEVar a])] }]]
|}]
;;

let%expect_test "parse adt constructor with arrow" =
  print_ast_p (parser "type t = C of int->int" |> Result.get_ok);
  [%expect
    {| [type_name t, type_params [], constructors [{ ctor_name = C; ctor_args = [TEArrow (TEInt, TEInt)] }]] |}]
;;

let%expect_test "parse adt tree declaration" =
  print_ast_p
    (parser
       {|type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree|}
     |> Result.get_ok);
  [%expect
    {|
    [type_name tree, type_params [a], constructors [{ ctor_name = Leaf; ctor_args = [TEVar a] }; { ctor_name = Node; ctor_args = [TEConstr (tree, [TEVar a]); TEConstr (tree, [TEVar a])] }]]
|}]
;;

let%expect_test "parse type application chain" =
  print_ast_p (parser "type t = Wrap of int list list" |> Result.get_ok);
  [%expect
    {| [type_name t, type_params [], constructors [{ ctor_name = Wrap; ctor_args = [TEConstr (list, [TEConstr (list, [TEInt])])] }]] |}]
;;

let%expect_test "parse match with constructor patterns" =
  print_ast_p
    (parser {|match Cons 1 Nil with | Cons x xs -> x | Nil -> 0|} |> Result.get_ok);
  [%expect
    {| Match (App (App (Constr "Cons", Int 1), Constr "Nil"), [(PConstr Cons, [Var x; Var xs], Var "x"); (PConstr Nil, [], Int 0)]) |}]
;;

let%expect_test "parse let with constructor pattern" =
  print_ast_p (parser {|let Cons x xs = Cons 1 Nil in xs|} |> Result.get_ok);
  [%expect
    {| Let (NonRec, "PConstr Cons, [Var x; Var xs]", App (App (Constr "Cons", Int 1), Constr "Nil"), Some Var "xs") |}]
;;
