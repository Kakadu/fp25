[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and Yackovlev Nickolay *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let main () =
  let input = In_channel.input_all In_channel.stdin in
  match Parser.parse input with
  | Error (Parser.Parsing_error err) -> Format.printf "Error: %s\n%!" err
  | Ok ast ->
    Format.printf "%a\n%!" Printast.pp_named ast;
    let fuel = 100_000 in
    (match Interpret.eval Interpret.initial_env fuel ast with
     | Ok (v, _fuel_left) -> Format.printf "%s\n%!" (Interpret.string_of_value v)
     | Error err -> Format.printf "Error: %s\n%!" (Interpret.string_of_error err))
;;

let () = main ()
