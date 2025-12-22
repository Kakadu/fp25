[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Stdio
open Lambda_lib

(** Parse command line arguments*)
let parse_args args =
  let rec parse max_steps = function
    | [] -> Some max_steps
    | "-max-steps" :: n :: rest ->
      (match Stdlib.int_of_string_opt n with
       | Some steps -> parse steps rest
       | None ->
         eprintf "Error: Invalid value for -max-steps: %s\n" n;
         None)
    | arg :: _ ->
      eprintf "Error: Unknown argument: %s\n" arg;
      None
  in
  match parse 10000 args with
  | Some max_steps -> max_steps
  | None -> Stdlib.exit 1
;;

let () =
  let max_steps = parse_args (List.tl_exn (Array.to_list (Sys.get_argv ()))) in
  let input = In_channel.input_all In_channel.stdin |> String.strip in
  match Parser.parse input with
  | Error e ->
    let msg = Stdlib.Format.asprintf "%a" Parser.pp_error e in
    eprintf "Error: %s\n" msg;
    Stdlib.exit 1
  | Result.Ok ast ->
    (match Interpret.eval_expr ~max_steps ast with
     | Result.Ok (Interpret.VInt n) -> printf "%d\n" n
     | Result.Ok (Interpret.VClosure _) -> printf "<fun>\n"
     | Result.Ok (Interpret.VBuiltin (name, _)) -> printf "<builtin:%s>\n" name
     | Result.Ok Interpret.VUnit -> printf "()\n"
     | Result.Error (`UnknownVariable name) ->
       eprintf "Unbound variable: %s\n" name;
       Stdlib.exit 1
     | Result.Error `DivisionByZero ->
       eprintf "Division by zero\n";
       Stdlib.exit 1
     | Result.Error `TypeMismatch ->
       eprintf "Type error\n";
       Stdlib.exit 1
     | Result.Error `StepLimitExceeded ->
       eprintf "Step limit exceeded\n";
       Stdlib.exit 1)
;;
