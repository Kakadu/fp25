[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type opts =
  { mutable show_ast : bool
  ; mutable max_steps : int
  }

let run_single opts input_text =
  (* Убираем пробелы и переводы строк с начала и конца текста *)
  let text = input_text |> String.trim in
  (* Пытаемся распарсить текст в AST *)
  match Parser.parse text with
  | Error (`Parsing_error msg) -> Format.printf "Parser error: %s\n" msg
  | Ok ast ->
    if opts.show_ast then Format.printf "AST: %a\n" Printast.pp_verbose ast;
    (match Interpret.eval ~step_limit:opts.max_steps () ast with
     | Ok value -> Format.printf "%s\n" (Interpret.string_of_value value)
     | Error err -> Format.eprintf "Error: %a\n" Interpret.pp_error err)
;;

let () =
  let opts = { show_ast = false; max_steps = 100_000 } in
  (* Ссылка для хранения имени входного файла (изначально None = читаем из stdin) *)
  let input_file = ref None in
  let arg_list =
    [ "--ast", Arg.Unit (fun () -> opts.show_ast <- true), "Show AST before evaluation"
    ; ( "--maxSteps"
      , Arg.Int (fun n -> opts.max_steps <- n)
      , "Set maximum number of evaluation steps (default: 100000)" )
    ]
  in
  Arg.parse arg_list (fun filename -> input_file := Some filename) "miniML interpreter";
  let input_text =
    match !input_file with
    | Some filename -> In_channel.(with_open_text filename input_all)
    | None -> In_channel.(input_all stdin)
  in
  run_single opts input_text
;;
