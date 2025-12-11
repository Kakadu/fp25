[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

module I = Interpret

let prompt = "# "

let is_quit_command (s : string) : bool =
  String.equal s ":q" || String.equal s ":quit" || String.equal s "quit"
;;

let rec repl_loop () =
  (* Print prompt and read a single line. *)
  Stdlib.print_string prompt;
  Stdlib.flush Stdlib.stdout;
  match Stdlib.read_line_opt () with
  | None ->
    (* End of input, finish REPL. *)
    ()
  | Some line ->
    let line = String.trim line in
    if String.equal line "" then
      (* Ignore empty lines. *)
      repl_loop ()
    else if is_quit_command line then
      (* Exit REPL on quit command. *)
      ()
    else (
      match I.run_program line with
      | Ok (v, _fuel_left) ->
        (* Print result in a simple OCaml-like form. *)
        Format.printf "- : %s\n%!" (I.string_of_value v)
      | Error err ->
        Format.printf "Error: %s\n%!" (I.string_of_run_error err));
      repl_loop ()
;;

let () = repl_loop ()
