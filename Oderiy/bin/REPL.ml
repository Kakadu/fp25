[@@@ocaml.text "/*"]

(** Copyright 2025, XRenso *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Mml

let () =
  let text = In_channel.(input_all stdin) |> String.trim in
  Interpret.parse_and_run text
;;
