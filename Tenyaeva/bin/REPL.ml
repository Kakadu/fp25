(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tenyaeva_lib.Parser
open Tenyaeva_lib.Ast

type options = { mutable dump_parsetree : bool }

let run_single options =
  let text = Stdlib.String.trim (In_channel.input_all stdin) in
  if not options.dump_parsetree
  then Stdlib.Format.printf "The interpreter is not yet implemented\n";
  if options.dump_parsetree
  then (
    match parse text with
    | Ok structure -> Stdlib.Format.printf "%s\n" (show_structure structure)
    | Error err -> Stdlib.Format.printf "%s\n" err)
;;

let () =
  let options = { dump_parsetree = false } in
  let () =
    let open Stdlib.Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> options.dump_parsetree <- true)
        , "Dump parse tree, don't eval enything" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Anonymous arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for MiniML Calculus"
  in
  run_single options
;;
