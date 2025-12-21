[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let () =
  let max_steps = ref 10000 in
  Stdlib.Arg.parse
    [ "-max-steps", Int (fun n -> max_steps := n), "Maximum evaluation steps" ]
    (fun _ -> failwith "No positional args")
    "miniML REPL";
  let input = In_channel.(input_all stdin) |> String.trim in
  match Parser.parse input with
  | Error e ->
    Format.printf "Error: %a\n%!" Parser.pp_error e;
    exit 1
  | Result.Ok ast ->
    (match Interpret.eval_expr ~max_steps:!max_steps ast with
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
