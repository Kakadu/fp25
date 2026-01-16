(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Filichkin_lib.Parser
open Filichkin_lib.Typecheck

let typecheck_program_str program =
  reset ();
  let toplevels = parser program |> Result.get_ok in
  match typecheck_program toplevels with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> string_of_type typ
     | None -> "No type")
  | Error err -> err
;;

let%expect_test "typecheck_list_head" =
  let program =
    {|
    type 'a mylist = Nil | Cons of 'a * 'a mylist;;
    let head xs =
      match xs with
      | Nil -> 0
      | Cons x _ -> x
    ;;
    head (Cons 1 (Cons 2 Nil))
  |}
  in
  print_string (typecheck_program_str program);
  [%expect {| int |}]
;;

let%expect_test "typecheck_polymorphic_option" =
  let program =
    {|
    let x = Some 1 in
    let y = Some true in
    y
  |}
  in
  print_string (typecheck_program_str program);
  [%expect {| option(bool) |}]
;;

let%expect_test "typecheck_exhaustive_custom" =
  let program =
    {|
    type color = Red | Green | Blue;;
    match Green with
    | Red -> 0
    | Green -> 1
    | Blue -> 2
  |}
  in
  print_string (typecheck_program_str program);
  [%expect {| int |}]
;;

let%expect_test "typecheck_non_exhaustive_custom" =
  let program =
    {|
    type color = Red | Green | Blue;;
    match Green with
    | Red -> 0
    | Green -> 1
  |}
  in
  print_string (typecheck_program_str program);
  [%expect {| non-exhaustive match, missing: Blue |}]
;;

let%expect_test "typecheck_unknown_type_in_decl" =
  print_string (typecheck_program_str "type t = C of nope");
  [%expect {| unknown type nope |}]
;;

let%expect_test "typecheck_type_arity_mismatch" =
  let program =
    {|
    type 'a box = Box of 'a;;
    type t = T of box
  |}
  in
  print_string (typecheck_program_str program);
  [%expect {| type constructor arity mismatch |}]
;;

let%expect_test "typecheck_match_wildcard" =
  print_string (typecheck_program_str "match Some 1 with | _ -> 0");
  [%expect {| int |}]
;;

let%expect_test "typecheck_constructor_value" =
  let program =
    {|
    type t = A | B of int;;
    let x = B 1 in x
  |}
  in
  print_string (typecheck_program_str program);
  [%expect {| t |}]
;;

let%expect_test "typecheck_constructor_function" =
  let program =
    {|
    type t = B of int;;
    let f = B in f
  |}
  in
  print_string (typecheck_program_str program);
  [%expect {| int -> t |}]
;;

let%expect_test "typecheck_tuple_pattern" =
  print_string (typecheck_program_str "let (x, y) = (1, true) in y");
  [%expect {| bool |}]
;;

let%expect_test "typecheck_unknown_constructor_expr" =
  print_string (typecheck_program_str "type t = A;; B");
  [%expect {| unknown constructor B |}]
;;

let%expect_test "typecheck_constructor_pattern_arity" =
  let program =
    {|
    type t = B of int;;
    match B 1 with
    | B x y -> x
  |}
  in
  print_string (typecheck_program_str program);
  [%expect {| constructor arity mismatch |}]
;;

let%expect_test "typecheck_type_application_expr" =
  let program =
    {|
    type 'a box = Box of 'a;;
    let x = Box 1 in x
  |}
  in
  print_string (typecheck_program_str program);
  [%expect {| box(int) |}]
;;
