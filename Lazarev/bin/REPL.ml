[@@@ocaml.text "*/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "*/*"]

module Arg = Stdlib.Arg
module Ast = Mini_ml_lib.Ast
module Interpreter = Mini_ml_lib.Interpreter
module Parser = Mini_ml_lib.Parser

type opts = { mutable env : Interpreter.env }

let get_input () =
  try Some (read_line ()) with
  | End_of_file -> None
;;

let run env =
  let line = get_input () in
  match line with
  | None -> ()
  | Some input ->
    (match Parser.parse_line input with
     | Parser.ParseError error -> Parser.show_error error |> print_endline
     | Parser.Parsed (ast, _) ->
       Interpreter.run env ast |> Interpreter.show_result |> print_endline)
;;

let () =
  let options = { env = Interpreter.new_env } in
  let args =
    [ ( "--steps"
      , Arg.Int (fun n -> options.env <- Interpreter.new_env_limited n)
      , "Set maximum number of evaluation steps (default: 1000)" )
    ]
  in
  Arg.parse args (fun _ -> ()) "";
  run options.env
;;
