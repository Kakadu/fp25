[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Stdio

let () =
  let expr =
    match Parser.parse (Stdio.In_channel.(input_all stdin) |> Base.String.rstrip) with
    | Ok expr ->
      let _ = Printf.printf "%s\n" (Print.print_ast expr) in
      expr
    | Error (`Parsing_error msg) -> failwith msg
  in
  match Interpret.run_interpret expr with
  | Ok expr -> Printf.printf "%s\n" (Print.print_ast expr)
  | Error er -> Printf.printf "%s\n" (Print.print_error er)
;;
