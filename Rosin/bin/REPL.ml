[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib
open Stdio
open Interpret

let string_of_value = function
  | VUnit -> "Unit"
  | VNum n -> string_of_int n
  | VClosure (_, _, _) -> "<closure>"
  | VRecClosure _ -> "<rec_closure>"
;;

let string_of_error = function
  | UnboundVariable x -> "UnboundVariable " ^ x
  | DivisionByZero -> "DivisionByZero"
  | StepLimitExceeded -> "StepLimitExceeded"
  | NonFunctionApplication v -> "NonFunctionApplication " ^ string_of_value v
  | InvalidUnop _ -> "InvalidUnop"
  | InvalidBinop _ -> "InvalidBinop"
  | TypeError -> "TypeError"
  | LetWithoutBody -> "LetWithoutBody"
  | LetrecWithoutBody -> "LetrecWithoutBody"
  | NonIntegerCondition _ -> "NonIntegerCondition"
;;

type args =
  { mutable ast : bool
  ; mutable steps : int
  }

let () =
  let pr_args = { ast = true; steps = max_int } in
  let arg_list =
    [ "--ast", Arg.Unit (fun () -> pr_args.ast <- false), ""
    ; "--steps", Arg.Int (fun n -> pr_args.steps <- n), ""
    ]
  in
  let usage_msg = "miniMl starts...\n" in
  Arg.parse arg_list (fun _ -> ()) usage_msg;
  Format.printf "AST here:\n";
  let expr =
    match Parser.parse (In_channel.(input_all stdin) |> Base.String.rstrip) with
    | Ok expr ->
      let _ = if pr_args.ast then Pprintast.pp Format.std_formatter expr in
      expr
    | Error (`Parsing_error msg) -> failwith msg
  in
  Format.printf "\nInterpretation result here:\n";
  match Interpret.run_interpret expr pr_args.steps with
  | Ok expr -> Format.printf "Ok: %s\n" (string_of_value expr)
  | Error er -> Format.printf "Error!: %s\n" (string_of_error er)
;;
