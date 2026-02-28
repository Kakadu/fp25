[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let usage () =
  Printf.eprintf
    "usage: %s [steps]\nsteps: positive integer, default 10000\n"
    Sys.argv.(0);
  exit 1
;;

let () =
  let steps =
    match Array.length Sys.argv with
    | 1 -> 10_000
    | 2 ->
      (match int_of_string_opt Sys.argv.(1) with
       | Some n when n > 0 -> n
       | _ -> usage ())
    | _ -> usage ()
  in
  let src = In_channel.input_all In_channel.stdin in
  match Interpret_lib.Interpret.run ~steps src with
  | Ok (v, output) ->
    List.iter print_endline output;
    print_endline (Interpret_lib.Interpret.format_value v)
  | Error e -> print_endline (Interpret_lib.Interpret.format_error e)
;;
