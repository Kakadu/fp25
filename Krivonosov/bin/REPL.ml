[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

(** Parse command line arguments*)
let parse_args args =
  let rec parse max_steps = function
    | [] -> Some max_steps
    | "-max-steps" :: n :: rest ->
      (match int_of_string_opt n with
       | Some steps -> parse steps rest
       | None ->
         Printf.eprintf "Error: Invalid value for -max-steps: %s\n" n;
         None)
    | arg :: _ ->
      Printf.eprintf "Error: Unknown argument: %s\n" arg;
      None
  in
  match parse 10000 args with
  | Some max_steps -> max_steps
  | None -> exit 1
;;

let () =
  let max_steps = parse_args (List.tl (Array.to_list Sys.argv)) in
  let input = In_channel.(input_all stdin) |> String.trim in
  match Parser.parse input with
  | Error e ->
    Format.printf "Error: %a\n%!" Parser.pp_error e;
    exit 1
  | Result.Ok ast ->
    (match Interpret.eval_expr ~max_steps ast with
     | Base.Result.Ok (Interpret.VInt n) -> Printf.printf "%d\n" n
     | Base.Result.Ok (Interpret.VClosure _) -> Printf.printf "<fun>\n"
     | Base.Result.Ok (Interpret.VBuiltin (name, _)) ->
       Printf.printf "<builtin:%s>\n" name
     | Base.Result.Ok Interpret.VUnit -> Printf.printf "()\n"
     | Base.Result.Error (`UnknownVariable name) ->
       Printf.eprintf "Unbound variable: %s\n" name;
       exit 1
     | Base.Result.Error `DivisionByZero ->
       Printf.eprintf "Division by zero\n";
       exit 1
     | Base.Result.Error `TypeMismatch ->
       Printf.eprintf "Type error\n";
       exit 1
     | Base.Result.Error `StepLimitExceeded ->
       Printf.eprintf "Step limit exceeded\n";
       exit 1)
;;
