[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type opts =
  { show_ast : bool
  ; max_steps : int
  }

let run_single opts input_text =
  let text = input_text |> String.trim in
  match Parser.parse text with
  | Error (`Parsing_error msg) -> Format.printf "Parser error: %s\n" msg
  | Ok ast ->
    if opts.show_ast then Format.printf "AST: %a\n" Printast.pp_verbose ast;
    (match Interpret.eval ~step_limit:opts.max_steps () ast with
     | Ok value -> Format.printf "%s\n" (Interpret.string_of_value value)
     | Error err -> Format.eprintf "Error: %a\n" Interpret.pp_error err)
;;

let parse_args () =
  let rec loop opts input_file args =
    match args with
    | [] -> opts, input_file
    | "--ast" :: rest -> loop { opts with show_ast = true } input_file rest
    | "--maxSteps" :: n :: rest ->
      (match int_of_string_opt n with
       | Some steps -> loop { opts with max_steps = steps } input_file rest
       | None ->
         Format.eprintf "Error: --maxSteps requires an integer argument\n";
         exit 1)
    | "--maxSteps" :: [] ->
      Format.eprintf "Error: --maxSteps requires an argument\n";
      exit 1
    | filename :: rest ->
      (match input_file with
       | None -> loop opts (Some filename) rest
       | Some _ ->
         Format.eprintf "Error: multiple input files specified\n";
         exit 1)
  in
  let default_opts = { show_ast = false; max_steps = 100_000 } in
  let args = List.tl (Array.to_list Sys.argv) in
  loop default_opts None args
;;

let () =
  let opts, input_file = parse_args () in
  let input_text =
    match input_file with
    | Some filename -> In_channel.(with_open_text filename input_all)
    | None -> In_channel.(input_all stdin)
  in
  run_single opts input_text
;;
