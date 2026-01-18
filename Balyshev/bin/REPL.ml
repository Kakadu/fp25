[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open MiniML
module Valuetree = Valuetree.Make (Monads.State) (Interpreter.Error)

let eval_expression, eval_structure =
  let open Interpreter.Make (Monads.State) in
  eval_expression, eval_structure
;;

let infer_expression, infer_structure =
  let open Inferencer.Make (Monads.State) in
  infer_expression, infer_structure
;;

type ty =
  | Expr
  | Stru
  | Patt
  | CoreType

type mode =
  | Parse
  | Eval
  | Infer

type opts =
  { mutable ty : ty
  ; mutable mode : mode
  ; mutable silent : bool
  }

open Stdlib.Format

let run_expr opts text =
  match Parser.parse_expression text with
  | Error err -> printf "parsing error: %s\n" err
  | Ok ast ->
    (match opts.mode with
     | Parse -> if not opts.silent then printf "parsed:@.%a\n" Parsetree.pp_expression ast
     | Eval ->
       (match eval_expression ast with
        | Error err -> printf "interpreter error: %a\n" Interpreter.Error.pp err
        | Ok value ->
          if not opts.silent then printf "evaluated: %a\n" Valuetree.pp_value value)
     | Infer ->
       (match infer_expression ast with
        | Error err -> printf "inferencer error: %a\n" Inferencer.Error.pp err
        | Ok ty -> if not opts.silent then printf "inferred: %a" Typedtree.pp_ty ty))
;;

let run_stru opts text =
  match Parser.parse_structure text with
  | Error err -> printf "parsing error: %s\n" err
  | Ok ast ->
    (match opts.mode with
     | Parse -> if not opts.silent then printf "parsed:@.%a\n" Parsetree.pp_structure ast
     | Infer ->
       (match infer_structure ast with
        | Ok typed_stru ->
          if not opts.silent
          then printf "typed structure:@.%a" Typedtree.pp_structure typed_stru
        | Error err -> printf "inferencer error: %a" Inferencer.Error.pp err)
     | Eval ->
       (match eval_structure ast with
        | Ok values ->
          if not opts.silent
          then printf "evaluation results:@.%a" Valuetree.pp_structure values
        | Error err -> printf "interpreter error: %a" Interpreter.Error.pp err))
;;

let run_patt opts text =
  match Parser.parse_pattern text with
  | Error err -> printf "parsing error: %s\n" err
  | Ok ast ->
    (match opts.mode with
     | Parse -> if not opts.silent then printf "parsed: %a\n" Parsetree.pp_pattern ast
     | Infer -> eprintf "not supported: infer pattern"
     | Eval -> eprintf "not supported: eval pattern")
;;

let run_core_type opts text =
  match Parser.parse_core_type text with
  | Error err -> printf "parsing error: %s\n" err
  | Ok ast ->
    (match opts.mode with
     | Parse -> if not opts.silent then printf "parsed: %a\n" Parsetree.pp_core_type ast
     | Infer -> eprintf "not supported: infer core-type"
     | Eval -> eprintf "not supported: eval core-type")
;;

let () =
  let opts = { ty = Expr; mode = Eval; silent = false } in
  let arg_mode v = Stdlib.Arg.Unit (fun () -> opts.mode <- v) in
  let arg_type v = Stdlib.Arg.Unit (fun () -> opts.ty <- v) in
  let silent b = Stdlib.Arg.Unit (fun () -> opts.silent <- b) in
  Stdlib.Arg.parse
    [ "-expr", arg_type Expr, "\t expression"
    ; "-stru", arg_type Stru, "\t structure"
    ; "-patt", arg_type Patt, "\t pattern"
    ; "-core-type", arg_type CoreType, "\t core type"
    ; "-parse", arg_mode Parse, "\t parser"
    ; "-eval", arg_mode Eval, "\t interpreter"
    ; "-infer", arg_mode Infer, "\t inferencer"
    ; ( "-silent"
      , silent true
      , "\t hide result (keeps primitives' side effects and error messages)" )
    ]
    (fun arg -> Stdlib.Format.eprintf "error: unexpected argument '%s'\n" arg)
    "REPL";
  (match opts.ty with
   | Expr -> run_expr opts
   | Patt -> run_patt opts
   | Stru -> run_stru opts
   | CoreType -> run_core_type opts)
    (Stdio.In_channel.(input_all stdin) |> String.rstrip)
;;
