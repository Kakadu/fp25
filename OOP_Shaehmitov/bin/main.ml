[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Interpreter
open Inferencer

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

type config =
  { filename : string option
  ; max_steps : int
  }

let default_config = { filename = None; max_steps = max_int }

let rec parse_args args config =
  match args with
  | [] -> Ok config
  | arg :: rest ->
    if String.starts_with ~prefix:"--maxsteps=" arg
    then (
      let prefix_len = String.length "--maxsteps=" in
      let num_str = String.sub arg prefix_len (String.length arg - prefix_len) in
      match int_of_string_opt num_str with
      | Some n ->
        if n > 0
        then parse_args rest { config with max_steps = n }
        else Error "Invalid integer for --maxsteps"
      | None -> Error "Invalid integer for --maxsteps")
    else if config.filename = None
    then parse_args rest { config with filename = Some arg }
    else Error "Multiple filenames provided"
;;

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  match parse_args args default_config with
  | Ok { filename = Some filename; max_steps } ->
    let source_code =
      try read_file filename with
      | Sys_error msg ->
        Printf.eprintf "File error: %s\n" msg;
        exit 1
    in
    (match Parser.parse_structure_items source_code with
     | Ok program ->
       (match TypeChecker.check_program_internal program with
        | Ok _ ->
          (match Interpreter.run_program max_steps program with
           | Ok _ -> ()
           | Error err -> Printf.eprintf "%s\n" (Interpreter.show_error err))
        | Error type_err ->
          Printf.eprintf
            "Type Error: %s\n"
            (Format.asprintf "%a" TypeChecker.pp_error type_err))
     | Error msg -> Printf.eprintf "Parse Error: %s\n" msg)
  | Ok { filename = None; _ } ->
    Printf.eprintf "Usage: %s [--maxsteps=<n>] <filename>\n" Sys.argv.(0)
  | Error msg ->
    Printf.eprintf "Error: %s\nUsage: %s [--maxsteps=<n>] <filename>\n" msg Sys.argv.(0)
;;
