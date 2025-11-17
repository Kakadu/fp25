[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Interpret

let () =
  let text = In_channel.(input_all stdin) |> String.trim in
  parse_and_run text
;;
