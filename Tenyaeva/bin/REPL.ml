(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tenyaeva_lib.Parser
open Tenyaeva_lib.Ast

type options =
  { mutable dump_parsetree : bool
  ; mutable dump_inference : bool
  }

let run_single options =
  let text = Stdlib.String.trim (In_channel.input_all stdin) in
  if not (options.dump_parsetree || options.dump_inference)
  then Stdlib.Format.printf "The interpreter is not yet implemented\n";
  if options.dump_parsetree
  then (
    match parse text with
    | Ok structure -> Stdlib.Format.printf "%s\n" (show_structure structure)
    | Error err ->
      Stdlib.Format.printf "%s\n" err;
      if options.dump_inference
      then Stdlib.Format.printf "The inferencer is not yet implemented\n")
;;

let () =
  let options = { dump_parsetree = false; dump_inference = false } in
  let () =
    let open Stdlib.Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> options.dump_parsetree <- true)
        , "Dump parse tree, don't eval enything" )
      ; ( "-dinference"
        , Unit (fun () -> options.dump_inference <- true)
        , "Eval and display type inference info" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Anonymous arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for MiniML Calculus"
  in
  run_single options
;;
