[@@@ocaml.text "*/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "*/*"]

module Arg = Stdlib.Arg
module Interpreter = Mini_ml_lib.Interpreter
module Parser = Mini_ml_lib.Parser

type opts = { mutable env : Interpreter.env }

let run env =
  while true do
    try
      let line = read_line () in
      match Parser.parse_line line with
      | Parser.ParseError error -> Parser.show_error error |> print_endline
      | Parser.Parsed (ast, _) ->
        Interpreter.run env ast |> snd |> Interpreter.show_result |> print_endline
    with
    | End_of_file -> exit 0
  done
;;

let () =
  let options = { env = Interpreter.new_env } in
  let args =
    [ ( "--steps"
      , Arg.Int (fun n -> options.env <- Interpreter.new_env_limited n)
      , "Set maximum number of evaluation steps" )
    ]
  in
  Arg.parse args (fun _ -> ()) "An interpreter for ML-like language";
  run options.env
;;
