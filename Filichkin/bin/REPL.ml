[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Filichkin_lib.Print
open Filichkin_lib.Parser
open Filichkin_lib.Interpret

let repl () =
  let input = read_line () in
  match parser input with
  | Ok ast ->
    let printed_ast = print_ast ast in
    Printf.printf "%s\n" printed_ast;
    (match run_interpret ast with
     | Ok value -> Printf.printf "%s\n\n" (string_of_value value)
     (* repl () *)
     | Error err -> Printf.printf "%s\n\n" (string_of_error err))
    (* repl () *)
  | Error (`parse_error msg) -> Printf.printf "Parse error: %s\n\n" msg
;;

(* repl () *)

let () = repl ()
