[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

type opts =
  { steps : int
  ; dump_parsetree : bool
  }

let default_opts = { steps = 10_000; dump_parsetree = false }

let parse_args argv =
  let rec loop opts = function
    | [] -> Ok opts
    | "-steps" :: n :: rest -> (
      match int_of_string_opt n with
      | Some steps -> loop { opts with steps } rest
      | None -> Error "Invalid value for -steps")
    | "-steps" :: [] -> Error "Missing value for -steps"
    | "-dparsetree" :: rest -> loop { opts with dump_parsetree = true } rest
    | _ :: _ -> Error "Positional arguments are not supported"
  in
  loop default_opts argv
;;

let () =
  let argv = Array.to_list Sys.argv |> List.tl in
  let opts =
    match parse_args argv with
    | Ok opts -> opts
    | Error msg ->
      Stdlib.Format.eprintf "%s\n%!" msg;
      Stdlib.exit 1
  in
  let input = In_channel.(input_all stdin) |> String.trim in
  if String.equal input ""
  then ()
  else (
    try
      let program = Parser.program_of_string input in
      if opts.dump_parsetree
      then Format.printf "Parsed: %s\n%!" (Ast.show_program program)
      else (
        match Interpret.eval_program ~fuel:opts.steps program with
        | Ok values ->
          List.iter (fun v -> print_endline (Interpret.string_of_value v)) values
        | Error err ->
          Printf.eprintf "Runtime error: %s\n%!" (Interpret.string_of_error err);
          Stdlib.exit 1)
    with
    | Parser.Error msg ->
      Printf.eprintf "Parse error: %s\n%!" msg;
      Stdlib.exit 1)
;;
