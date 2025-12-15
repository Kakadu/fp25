(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lambda_lib
open Parser
open Interpret
open Printast

type opts =
  { mutable show_ast : bool
  ; mutable max_steps : int
  }

let string_of_value = function
  | VInt n -> string_of_int n
  | VUnit -> "()"
  | VClosure _ -> "<closure>"
  | VRecClosure _ -> "<rec_closure>"
  | VBuiltin _ -> "<builtin>"
;;

let string_of_error = function
  | UnboundVariable name -> Printf.sprintf "Unbound variable: %s" name
  | TypeError msg -> Printf.sprintf "Type error: %s" msg
  | DivisionByZero -> "Division by zero"
  | NotAFunction -> "Not a function"
  | ExceedMaxSteps -> "Maximum steps exceeded"
;;

let run_single opts =
  let text = In_channel.(input_all stdin) |> String.trim in
  match parse text with
  | Error (`Parsing_error msg) -> Format.printf "Parser error: %s\n" msg
  | Ok ast ->
    if opts.show_ast then Format.printf "AST: %a\n" pp_expr ast;
    (match run_interpreter ast opts.max_steps with
     | Ok value -> Format.printf "Result: %s\n" (string_of_value value)
     | Error err -> Format.printf "Error: %s\n" (string_of_error err))
;;

let () =
  let opts = { show_ast = false; max_steps = 1000 } in
  let arg_list =
    [ "--ast", Arg.Unit (fun () -> opts.show_ast <- true), "Show AST before evaluation"
    ; ( "--maxSteps"
      , Arg.Int (fun n -> opts.max_steps <- n)
      , "Set maximum number of evaluation steps (default: 1000)" )
    ]
  in
  Arg.parse arg_list (fun _ -> ()) "";
  run_single opts
;;
