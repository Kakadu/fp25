[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open DMotuzov_lib.Interpreter
open DMotuzov_lib.Parser

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

type config =
  { filename : string option
  ; maxsteps : int
  }

let default_config = { filename = None; maxsteps = 10000 }

let rec parse_args args config =
  match args with
  | [] -> Ok config
  | arg :: rest ->
    if String.starts_with ~prefix:"--maxsteps=" arg
    then (
      let steps_str = String.sub arg 11 (String.length arg - 11) in
      try
        let steps = int_of_string steps_str in
        parse_args rest { config with maxsteps = steps }
      with
      | Failure _ -> Error ("Invalid number for maxsteps: " ^ steps_str))
    else if config.filename = None
    then parse_args rest { config with filename = Some arg }
    else Error "Multiple filenames provided"
;;

let show_runtime_error = function
  | TypeError -> "Error: Type error"
  | DivisionByZero -> "Error: Division by zero"
  | NoVariable id -> "Error: Unknown variable: " ^ id
  | OutOfMaxSteps -> "Error: Maximum evaluation steps exceeded"
;;

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  match parse_args args default_config with
  | Ok { filename = Some filename; maxsteps } ->
    let source_code =
      try read_file filename with
      | Sys_error msg ->
        Printf.eprintf "File error: %s\n" msg;
        exit 1
    in
    (match parse source_code with
     | Error msg -> Printf.eprintf "Parse Error: %s\n" msg
     | Ok program ->
       (match run_interpreter program maxsteps with
        | Ok _env -> ()
        | Error err -> Printf.eprintf "%s\n" (show_runtime_error err)))
  | Ok { filename = None; _ } ->
    Printf.eprintf "Usage: %s <filename> [--maxsteps=N]\n" Sys.argv.(0)
  | Error msg ->
    Printf.eprintf "Error: %s\nUsage: %s <filename> [--maxsteps=N]\n" msg Sys.argv.(0)
;;
