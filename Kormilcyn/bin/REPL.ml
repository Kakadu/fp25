[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib

let extract_steps line =
  let prefix = ":max_steps" in
  let rest =
    String.sub line (String.length prefix) (String.length line - String.length prefix)
    |> String.trim
  in
  int_of_string rest
;;

let print_help () =
  print_endline "MiniML REPL";
  print_endline "Commands: :help, :quit, :q, :max_steps <UNSIGNED INT>";
  print_endline "Enter an expression and press Enter to evaluate."
;;

let run_line max_steps line = Interpret.parse_and_run ~max_steps line

let rec repl ~(max_steps : int) =
  print_string "miniml> ";
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
    let line = String.trim line in
    if String.equal line ""
    then repl ~max_steps
    else if String.equal line ":help"
    then (
      print_help ();
      repl ~max_steps)
    else if String.equal line ":quit" || String.equal line ":q"
    then ()
    else if String.starts_with ~prefix:":max_steps" line
    then repl ~max_steps:(extract_steps line)
    else (
      run_line max_steps line;
      repl ~max_steps)
;;

let () =
  print_help ();
  repl ~max_steps:10000
;;
