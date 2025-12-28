(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* open Tenyaeva_lib.Ast *)
open Tenyaeva_lib.Parser
open Tenyaeva_lib.Pprinter
open Generators

let arbitrary_structure =
  QCheck.make gen_structure ~print:(Format.asprintf "%a" pp_structure)
;;

let run n =
  QCheck_base_runner.run_tests
    [ QCheck.Test.make ~count:n arbitrary_structure (fun s ->
        (* Stdlib.Format.printf "%s\n" (show_structure s); *)
        Result.ok s = parse (Format.asprintf "%a" pp_structure s))
    ]
;;

let () =
  let _ = run 3 in
  ()
;;
