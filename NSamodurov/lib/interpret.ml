[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)

open Parser
open Inferencer
open Compiler

type eval =
  | Int of int
  | Epsilon
[@@deriving show { with_path = false }]

let interpret =
  let rec helper acc env arg ret = function
    | [] -> acc
    | Const a :: Add :: tl ->
      let b =
        match acc with
        | Epsilon -> failwith "error"
        | Int i -> i
      in
      helper (Int (a + b)) env arg ret tl
    | Const i :: tl -> helper (Int i) env arg ret tl
    | Push :: tl -> helper acc env (acc :: arg) ret tl
    | PushMark :: tl -> helper acc env (Epsilon :: arg) ret tl
    | _ -> failwith "unimpl"
  in
  helper Epsilon [] [] []
;;

let parse_and_run str =
  let helper str =
    let ( let* ) = Result.bind in
    let* ast = parse str in
    let ast = to_brujin ast in
    let _ = w ast in
    let instr = compile ast in
    Result.ok (interpret instr)
  in
  match helper str with
  | Ok v -> Format.printf "Success: %a" pp_eval v
  | Error e -> Format.printf "Error: %a" Inferencer.pp_error e
;;
