(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Filichkin_lib.Parser
open Filichkin_lib.Typecheck

let%expect_test "typecheck_fibonacci" =
  reset ();
  let toplevels =
    parser
      "let rec fib n = if n <> 0 then if (n - 1) <> 0 then fib (n - 1) + fib (n - 2) \
       else 1 else 0 in fib 10"
    |> Result.get_ok
  in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| int |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;

let%expect_test "typecheck_factorial" =
  reset ();
  let toplevels =
    parser "let rec fac = fun x -> if x = 1 then 1 else x * fac(x-1) in fac 5"
    |> Result.get_ok
  in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| int |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;

let%expect_test "typecheck_function_type" =
  reset ();
  let toplevels = parser "let add x y = x + y in add" |> Result.get_ok in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| int -> int -> int |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;

let%expect_test "typecheck_partial_application" =
  reset ();
  let toplevels =
    parser "let add x y = x + y in let add5 = add 5 in add5" |> Result.get_ok
  in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| int -> int |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;

let%expect_test "typecheck_tuple" =
  reset ();
  let toplevels = parser "(1, true, 3)" |> Result.get_ok in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| (int * bool * int) |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;

let%expect_test "typecheck_type_error_int_bool_addition" =
  reset ();
  let toplevels = parser "5 + true" |> Result.get_ok in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    print_string "Unexpected success";
    [%expect.unreachable]
  | Error err ->
    print_string err;
    [%expect {| type mismatch |}]
;;

let%expect_test "typecheck_type_error_if_condition" =
  reset ();
  let toplevels = parser "if 5 then 1 else 2" |> Result.get_ok in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    print_string "Unexpected success";
    [%expect.unreachable]
  | Error err ->
    print_string err;
    [%expect {| type mismatch |}]
;;

let%expect_test "typecheck_type_error_branch_mismatch" =
  reset ();
  let toplevels = parser "if true then 1 else false" |> Result.get_ok in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    print_string "Unexpected success";
    [%expect.unreachable]
  | Error err ->
    print_string err;
    [%expect {| type mismatch |}]
;;

let%expect_test "typecheck_match_exhaustive" =
  reset ();
  let toplevels = parser "match None with | None -> 0 | Some x -> x" |> Result.get_ok in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| int |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;

let%expect_test "typecheck_match_non_exhaustive" =
  reset ();
  let toplevels = parser "match None with | Some x -> x" |> Result.get_ok in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    print_string "Unexpected success";
    [%expect.unreachable]
  | Error err ->
    print_string err;
    [%expect {| non-exhaustive match, missing: None |}]
;;

let%expect_test "typecheck_recursive_function" =
  reset ();
  let toplevels =
    parser "let rec f x = if x = 0 then 1 else f (x - 1) in f" |> Result.get_ok
  in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| int -> int |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;

let%expect_test "typecheck_polymorphic_identity" =
  reset ();
  let toplevels = parser "let id x = x in (id 5, id true)" |> Result.get_ok in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| (int * bool) |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;

let%expect_test "typecheck_custom_type_definition" =
  reset ();
  let toplevels =
    parser "type 'a mylist = Nil | Cons of 'a * 'a mylist;; let x = Cons(1, Nil)"
    |> Result.get_ok
  in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| int mylist |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;

let%expect_test "typecheck_complex_function_type" =
  reset ();
  let toplevels = parser "let compose f g x = f (g x) in compose" |> Result.get_ok in
  let res = typecheck_program toplevels in
  match res with
  | Ok () ->
    (match get_last_type () with
     | Some typ -> print_string (string_of_type typ)
     | None -> print_string "No type");
    [%expect {| ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b |}]
  | Error err ->
    print_string err;
    [%expect.unreachable]
;;
