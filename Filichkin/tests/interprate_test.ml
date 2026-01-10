(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Filichkin_lib.Parser
open Filichkin_lib.Interpret
open Filichkin_lib.Ast

let extract_expr_from_toplevels = function
  | [ TLExpr e ] -> e
  | _ -> failwith "Expected single expression"
;;

let%expect_test "Fibonacci" =
  let toplevels =
    parser
      "let rec fib n = if n <> 0 then if (n - 1) <> 0 then fib (n - 1) + fib (n - 2) \
       else 1 else 0 in fib 10"
    |> Result.get_ok
  in
  let expr = extract_expr_from_toplevels toplevels in
  let res = run_interpret expr in
  match res with
  | Ok value ->
    print_string (string_of_value value);
    [%expect {| 55 |}]
  | Error e ->
    print_string (string_of_error e);
    [%expect.unreachable]
;;

let%expect_test "factorial" =
  let toplevels =
    parser "let rec fac = fun x -> if x = 1 then 1 else x * fac(x-1) in fac 5"
    |> Result.get_ok
  in
  let expr = extract_expr_from_toplevels toplevels in
  let res = run_interpret expr in
  match res with
  | Ok value ->
    print_string (string_of_value value);
    [%expect {|120|}]
  | Error e ->
    print_string (string_of_error e);
    [%expect.unreachable]
;;

let%expect_test "1" =
  let toplevels = parser "let r x y= y+x*8 in r 9 10" |> Result.get_ok in
  let expr = extract_expr_from_toplevels toplevels in
  let res = run_interpret expr in
  match res with
  | Ok value ->
    print_string (string_of_value value);
    [%expect {|82|}]
  | Error e ->
    print_string (string_of_error e);
    [%expect.unreachable]
;;

let%expect_test "2" =
  let toplevels =
    parser
      "let r = (fun s k -> s+k) 5 7 in let p = (fun s-> s*2) ((fun k -> k*3) 10) in p/2 \
       + r"
    |> Result.get_ok
  in
  let expr = extract_expr_from_toplevels toplevels in
  let res = run_interpret expr in
  match res with
  | Ok value ->
    print_string (string_of_value value);
    [%expect {|42|}]
  | Error e ->
    print_string (string_of_error e);
    [%expect.unreachable]
;;

let%expect_test "3" =
  let toplevels =
    parser "let add x y = x + y in let add5 = add 5 in add5 3 + add5 2" |> Result.get_ok
  in
  let expr = extract_expr_from_toplevels toplevels in
  let res = run_interpret expr in
  match res with
  | Ok value ->
    print_string (string_of_value value);
    [%expect {| 15 |}]
  | Error e ->
    print_string (string_of_error e);
    [%expect.unreachable]
;;
