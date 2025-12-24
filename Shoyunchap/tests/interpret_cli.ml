(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lambda_lib

let parse_args () =
  let rec loop steps acc = function
    | [] -> steps, List.rev acc
    | ("--steps" | "--fuel") :: n :: tl -> loop (Some (int_of_string n)) acc tl
    | opt :: tl ->
      (match String.split_on_char '=' opt with
       | [ "--steps"; n ] -> loop (Some (int_of_string n)) acc tl
       | _ -> loop steps (opt :: acc) tl)
  in
  loop None [] (List.tl (Array.to_list Sys.argv))
;;

let read_stdin () =
  let buf = Buffer.create 128 in
  (try
     while true do
       Buffer.add_string buf (input_line stdin);
       Buffer.add_char buf '\n'
     done
   with
   | End_of_file -> ());
  Buffer.contents buf
;;

let () =
  let steps_override, parts = parse_args () in
  let expr =
    match parts with
    | [] -> read_stdin ()
    | _ -> String.concat " " parts
  in
  let max_steps =
    match steps_override with
    | Some n -> n
    | None -> Interpret.max_steps_from_env 100_000
  in
  match Parser.parse expr with
  | Error e -> Format.printf "%a@." Parser.pp_error e
  | Ok ast ->
    (match Interpret.run ~max_steps ast with
     | Ok v -> Printf.printf "%s\n%!" (Interpret.string_of_value v)
     | Error err -> Printf.printf "Error: %s\n%!" (Interpret.string_of_error err))
;;
