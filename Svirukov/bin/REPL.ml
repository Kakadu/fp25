[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let expression =
  "let result =\n\
  \  let sterter x = if x + (8 * 9) > 4 then x * 2 else x * 5 in\n\
  \    let inner pp = sterter pp + 8 in\n\
  \  inner 8"
;;

let () =
  match Parser.parse expression with
  | Ok expr -> Printf.printf "%s\n" (Print.printer expr)
  | Error (`Parsing_error msg) -> Printf.printf "Parse error: %s\n" msg
;;
