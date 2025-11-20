[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Parser
open Compiler

let instruction_of_program str =
  let helper str =
    let ( let* ) = Result.bind in
    let* ast = parse str in
    let ast = to_brujin ast in
    let instr = compile ast in
    Result.ok instr
  in
  match helper str with
  | Ok v -> List.iter (fun x -> Format.printf "%a;" Compiler.pp_instr x) v
  | Error e -> Format.printf "Error: %a" Inferencer.pp_error e
;;

let%expect_test "applicaiton1" =
  instruction_of_program "let x = 1 in x";
  [%expect {| (Const 1);Let;(Access 0);EndLet; |}]
;;

let%expect_test "constant" =
  instruction_of_program "1";
  [%expect {| (Const 1); |}]
;;

let%expect_test "arith" =
  instruction_of_program "(1 + 2)";
  [%expect {| (Const 2);Push;(Const 1);Add; |}]
;;

let%expect_test "function" =
  instruction_of_program "fun x -> (2 + 1)";
  [%expect {| (Cur [(Const 1); Push; (Const 2); Add; Return]); |}]
;;

let%expect_test "identity" =
  instruction_of_program "let id = (fun x -> x) in id 1";
  [%expect
    {| (Cur [(Access 0); Return]);Let;PushMark;(Const 1);Push;(Access 0);Apply;EndLet; |}]
;;

let%expect_test "identity" =
  instruction_of_program " let f = (fun x -> 5 + 2) in f 7";
  [%expect
    {|
    (Cur [(Const 2); Push; (Const 5); Add; Return]);Let;PushMark;(Const 7);Push;(
    Access 0);Apply;EndLet;
    |}]
;;
