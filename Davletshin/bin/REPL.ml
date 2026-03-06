[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib

let interactive = Unix.isatty Unix.stdin

let rec repl () =
  if interactive then print_string ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | line ->
    let text = String.trim line in
    if not (String.equal text "") then Interpret.parse_and_run text 1000000;
    repl ()
;;

let () =
  let open Stdlib.Arg in
  parse
    []
    (fun _ ->
      Stdlib.Format.eprintf "Positioned arguments are not supported\n";
      Stdlib.exit 1)
    "Read-Eval-Print-Loop for Utyped Lambda Calculus"
in
repl ()
