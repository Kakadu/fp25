[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let steps =
  match Array.length Sys.argv with
  | 2 when int_of_string Sys.argv.(1) >= 0 -> int_of_string Sys.argv.(1)
  | _ -> 1000
;;

let parsed = Parser.parse (In_channel.input_all stdin |> Base.String.rstrip) in
match parsed with
| Error (`Parsing_error _) -> Printf.printf "%s\n" "Failed to parse"
| Ok x ->
  let interpreted = Interpret.eval Interpret.init steps x in
  (match interpreted with
   | Ok (EVal x) -> Printf.printf "\n%d\n" x
   | Error e -> Printf.printf "\n%s\n" e
   | _ -> Printf.printf "\n%s\n" "Non integer result")
