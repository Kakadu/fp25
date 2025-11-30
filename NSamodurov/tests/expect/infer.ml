[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Parser
open Type
open Inferencer
open Utils

let parse_and_print str =
  let helper str =
    let ( let* ) = Result.bind in
    let* ast = parse str in
    let ast = to_brujin ast in
    w ast
  in
  match helper str with
  | Ok v -> Format.printf "Type: %a" pp_ty v
  | Error e -> Format.printf "Error: %a" pp_error e
;;

let%expect_test "one int" =
  parse_and_print "42";
  [%expect {| Type: (TGround "int") |}]
;;

let%expect_test "arithmetic expression" =
  parse_and_print "(1 + 2)";
  [%expect {| Type: (TGround "int") |}]
;;

let%expect_test "constant function" =
  parse_and_print "fun x -> 1";
  [%expect {| Type: (TArrow ((TVar 0), (TGround "int"))) |}]
;;

let%expect_test "identity" =
  parse_and_print "fun x -> x";
  [%expect {| Type: (TArrow ((TVar 0), (TVar 0))) |}]
;;

let%expect_test "false" =
  parse_and_print "fun x y -> y";
  [%expect {| Type: (TArrow ((TVar 0), (TArrow ((TVar 1), (TVar 1))))) |}]
;;

let%expect_test "many arguments" =
  parse_and_print "fun x y z u v -> y";
  [%expect
    {|
    Type: (TArrow ((TVar 0),
             (TArrow ((TVar 1),
                (TArrow ((TVar 2),
                   (TArrow ((TVar 3), (TArrow ((TVar 4), (TVar 1)))))))
                ))
             ))
    |}]
;;

let%expect_test "let1" =
  parse_and_print "let x = 1 in 2";
  [%expect {| Type: (TGround "int") |}]
;;

let%expect_test "let2" =
  parse_and_print "let x = 1 in x";
  [%expect {| Type: (TGround "int") |}]
;;

let%expect_test "basic substituion" =
  parse_and_print "let id x = x in id 1";
  [%expect {| Type: (TGround "int") |}]
;;

let%expect_test "compare" =
  parse_and_print "if true then 1 else 2";
  [%expect {| Type: (TGround "int") |}]
;;

let%expect_test "compare" =
  parse_and_print "let cmp x y = x < y in cmp";
  [%expect
    {|
    Type: (TArrow ((TGround "int"), (TArrow ((TGround "int"), (TGround "bool")))
             ))
    |}]
;;

let%expect_test "arith" =
  parse_and_print "let arith x y = x + y in arith";
  [%expect
    {| Type: (TArrow ((TGround "int"), (TArrow ((TGround "int"), (TGround "int"))))) |}]
;;

let%expect_test "factorial" =
  parse_and_print "let rec id x = if (true) then 0 else id (x-1) in id";
  [%expect
    {|
    Error: Unification error: ((TGround "int")) ((TArrow ((TGround "int"),
                                                    (TGround "int"))))
    |}]
;;

let%expect_test "many arguments" =
  parse_and_print "fun x y z u v -> y";
  [%expect
    {|
    Type: (TArrow ((TVar 0),
             (TArrow ((TVar 1),
                (TArrow ((TVar 2),
                   (TArrow ((TVar 3), (TArrow ((TVar 4), (TVar 1)))))))
                ))
             ))
    |}]
;;
