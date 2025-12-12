(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tenyaeva_lib.Ast
open Tenyaeva_lib.Parser
open Tenyaeva_lib.Pprinter

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

let run_tests n =
  let _ = run n in
  ()
;;

let () =
  Arg.parse
    [ "-seed", Arg.Int QCheck_base_runner.set_seed, " Set seed"
    ; "-gen", Arg.Int run_tests, " Number of runs"
    ]
    (fun _ -> assert false)
    "help"
;;
