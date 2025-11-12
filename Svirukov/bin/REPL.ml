[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let expression = "5 > 8 - 8"

let () = 
match Parser.parse expression with 
| Ok expr -> Printf.printf "%s\n" (Parser.printer expr)
| Error _ -> Printf.printf "Error"
