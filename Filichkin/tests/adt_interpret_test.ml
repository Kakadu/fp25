(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Filichkin_lib.Parser
open Filichkin_lib.Interpret

let run_program program =
  let toplevels = parser program |> Result.get_ok in
  match interpret_program initial_state toplevels with
  | Ok (_, Some value) -> string_of_value value
  | Ok (_, None) -> "No result"
  | Error err -> string_of_error err
;;

let%expect_test "interpret_list_sum" =
  let program =
    {|
    type intlist = Nil | Cons of int * intlist;;
    let rec sum xs =
      match xs with
      | Nil -> 0
      | Cons x tl -> x + sum tl
    ;;
    sum (Cons 1 (Cons 2 (Cons 3 Nil)))
  |}
  in
  print_string (run_program program);
  [%expect {| 6 |}]
;;

let%expect_test "interpret_tree_size" =
  let program =
    {|
    type tree = Leaf of int | Node of tree * tree;;
    let rec size t =
      match t with
      | Leaf _ -> 1
      | Node l r -> size l + size r
    ;;
    size (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
  |}
  in
  print_string (run_program program);
  [%expect {| 3 |}]
;;

let%expect_test "interpret_color_match_wildcard" =
  let program =
    {|
    type color = Red | Green | Blue;;
    match Green with
    | Red -> 0
    | _ -> 1
  |}
  in
  print_string (run_program program);
  [%expect {| 1 |}]
;;

let%expect_test "interpret_list_length" =
  let program =
    {|
    type intlist = Nil | Cons of int * intlist;;
    let rec length xs =
      match xs with
      | Nil -> 0
      | Cons _ tl -> 1 + length tl
    ;;
    length (Cons 1 (Cons 2 Nil))
  |}
  in
  print_string (run_program program);
  [%expect {| 2 |}]
;;

let%expect_test "interpret_constructor_partial_application" =
  let program =
    {|
    type intlist = Nil | Cons of int * intlist;;
    let cons1 = Cons 1 in
    cons1 Nil
  |}
  in
  print_string (run_program program);
  [%expect {| Cons 1 Nil |}]
;;

let%expect_test "interpret_sequential_toplevels" =
  let program = {|
    let x = 1;;
    let y = 2;;
    x + y
  |} in
  print_string (run_program program);
  [%expect {| 3 |}]
;;

let%expect_test "interpret_non_exhaustive_match" =
  print_string (run_program "match None with | Some x -> x");
  [%expect {| Type error: non-exhaustive pattern matching |}]
;;

let%expect_test "interpret_nested_constructor_match" =
  let program =
    {|
    type int_opt = INone | ISome of int;;
    match ISome 5 with
    | INone -> 0
    | ISome x -> x
  |}
  in
  print_string (run_program program);
  [%expect {| 5 |}]
;;

let%expect_test "interpret_tuple_pattern" =
  print_string (run_program "let (x, y) = (1, 2) in x + y");
  [%expect {| 3 |}]
;;

let%expect_test "interpret_let_wildcard" =
  print_string (run_program "let _ = 5 in 1");
  [%expect {| 1 |}]
;;
