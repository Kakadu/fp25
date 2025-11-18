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
  let expr =
    match Parser.parse "let _ = 8" with
    | Ok expr ->
      (*let _ = Printf.printf "%d\n" r in*)
      let _ = Printf.printf "%s\n" (Print.print_ast expr) in
      expr
    | Error (`Parsing_error msg) -> failwith msg
  in
  match Interpret.run_interpret expr with
  | Ok expr -> Printf.printf "%s\n" (Print.print_ast expr)
  | Error er -> Printf.printf "%s\n" (Print.print_error er)
;;
