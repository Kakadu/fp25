(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tenyaeva_lib.Parser
open Tenyaeva_lib.Interpreter
open Tenyaeva_lib.Ast

type options =
  { mutable dump_parsetree : bool
  ; mutable max_steps : int
  }

let run_single options =
  let text = Stdlib.String.trim (In_channel.input_all stdin) in
  if not options.dump_parsetree
  then (
    match parse text with
    | Ok structure ->
      (match run_interpreter structure options.max_steps with
       | Ok out_list ->
         List.iter
           (function
             | Some id, val' -> Format.printf "val %s = %a\n" id pp_value val'
             | None, val' -> Format.printf "%a\n" pp_value val')
           out_list
       | Error err -> Stdlib.Format.printf "%a\n" pp_eval_error err)
    | Error err -> Stdlib.Format.printf "%s\n" err);
  if options.dump_parsetree
  then (
    match parse text with
    | Ok structure -> Stdlib.Format.printf "%s\n" (show_structure structure)
    | Error err -> Stdlib.Format.printf "%s\n" err)
;;

let () =
  let options = { dump_parsetree = false; max_steps = 10_000 } in
  let () =
    let open Stdlib.Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> options.dump_parsetree <- true)
        , "Dump parse tree, don't eval enything" )
      ; ( "-max-steps"
        , Arg.Int (fun n -> options.max_steps <- n)
        , "Set a limit on interpretation steps (default: 10_000)" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Anonymous arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for MiniML Calculus"
  in
  run_single options
;;
