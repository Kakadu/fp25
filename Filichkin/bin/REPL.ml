[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Filichkin_lib.Print
open Filichkin_lib.Parser
open Filichkin_lib.Interpret
open Filichkin_lib.Typecheck
module Interpret = Filichkin_lib.Interpret
module Typecheck = Filichkin_lib.Typecheck

let read_one_line () =
  try Some (read_line ()) with
  | End_of_file -> None
;;

let repl () =
  match read_one_line () with
  | None -> ()
  | Some input ->
    (match parser input with
     | Error (`parse_error msg) -> Printf.printf "Parse error: %s\n" msg
     | Ok toplevels ->
       print_ast_p toplevels;
       (match typecheck_program Typecheck.initial_state toplevels with
        | Error type_err -> Printf.printf "Type error: %s\n" type_err
        | Ok tc_state ->
          (match get_last_type tc_state with
           | Some ty -> Printf.printf "Type: %s\n" (string_of_type ty)
           | None -> ());
          (match interpret_program Interpret.initial_state toplevels with
           | Error err -> Printf.printf "%s\n" (string_of_error err)
           | Ok (_, last_result) ->
             (match last_result with
              | Some v -> Printf.printf "%s\n" (string_of_value v)
              | None -> ());
             Printf.printf "\n")))
;;

let () = repl ()
