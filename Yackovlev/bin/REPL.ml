[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

module I = Interpret

let prompt = "# "

let rec repl_loop () =
  (* Print prompt and flush stdout immediately using %! *)
  Format.printf "%s%!" prompt;
  match Stdlib.read_line () with
  | exception End_of_file ->
    (* Handle Ctrl+D (EOF) gracefully to exit the loop *)
    ()
  | line ->
    let line = String.trim line in
    if String.equal line "quit"
    then
      (* Exit on specific command *)
      ()
    else (
      (* Process input if it's not empty *)
      (if String.length line > 0
       then
         match I.run_program line with
         | Ok (v, _fuel) -> Format.printf "- : %s\n%!" (I.string_of_value v)
         | Error err -> Format.printf "Error: %s\n%!" (I.string_of_run_error err));
      repl_loop ())
;;

let () = repl_loop ()
