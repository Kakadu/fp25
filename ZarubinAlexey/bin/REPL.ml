[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base

let read_all_stdin () : string = In_channel.input_all In_channel.stdin

let () =
  let input = read_all_stdin () in
  (* Сейчас мы используем твой существующий entrypoint *)
  Lambda_lib.Interpret.parse_and_run input
;;
