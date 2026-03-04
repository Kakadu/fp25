(** Copyright 2026, ChurinNick *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lambda_lib

let default_steps = 50_000

let print_result = function
  | Ok v -> print_endline (Interpret.string_of_value v)
  | Error e -> print_endline ("Error: " ^ Interpret.string_of_error e)
;;

let process_input steps input =
  let trimmed = String.trim input in
  if trimmed = "" || trimmed = "quit" || trimmed = "exit"
  then None
  else (
    match Parser.parse trimmed with
    | Error (`Parsing_error msg) ->
      print_endline ("Parse error: " ^ msg);
      Some steps
    | Ok ast ->
      print_result (Interpret.eval_expr ~steps ast);
      Some steps)
;;

let rec repl steps =
  flush stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> ()
  | Some line ->
    (match process_input steps line with
     | None -> ()
     | Some new_steps -> repl new_steps)
;;

let parse_args () =
  let steps = ref default_steps in
  let args = Array.to_list Sys.argv in
  (match args with
   | _ :: "-steps" :: n :: _ ->
     (match int_of_string_opt n with
      | Some s -> steps := s
      | None -> ())
   | _ -> ());
  !steps
;;

let () =
  let steps = parse_args () in
  if steps <> default_steps then Printf.printf "Step limit: %d\n" steps;
  repl steps
;;
