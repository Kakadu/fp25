[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let prompt = "> "

let read_max_steps () =
  print_string "max steps> ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> 1000
  | line ->
    let line = String.trim line in
    (match int_of_string_opt line with
     | Some n when n > 0 -> n
     | _ -> 1000)
;;

let read_printer_flag () =
  print_string "ast printer> ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> false
  | line ->
    let line = String.trim line in
    (match bool_of_string_opt line with
     | Some true -> true
     | _ -> false)
;;

let rec repl max_steps printer_flag =
  print_string prompt;
  flush stdout;
  match read_line () with
  | exception End_of_file -> print_newline ()
  | line ->
    let line = String.trim line in
    if line = ":q"
    then ()
    else (
      if line <> "" then Interpret.run_eval line max_steps printer_flag;
      repl max_steps printer_flag)
;;

let () =
  let max_steps = read_max_steps () in
  let printer_flag = read_printer_flag () in
  repl max_steps printer_flag
;;
