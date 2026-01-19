(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tenyaeva_lib.Parser
open Tenyaeva_lib.Pprinter
open Generators
open Stdlib.Format

let arbitrary_structure =
  QCheck.make gen_structure ~print:(Format.asprintf "%a" pp_structure)
;;

let test_round_trip =
  QCheck.Test.make
    ~name:"AST -> pretty-printer -> parser -> AST"
    ~count:10_000
    arbitrary_structure
    (fun structure ->
       let printed_structure = asprintf "%a" pp_structure structure in
       match parse printed_structure with
       | Ok parsed -> parsed = structure
       | Error err ->
         printf "Generated program:\n%s\n\n" printed_structure;
         printf "Parsing failed with error: %s\n" err;
         false)
;;

let () =
  let _ : int = QCheck_base_runner.run_tests [ test_round_trip ] in
  ()
;;
