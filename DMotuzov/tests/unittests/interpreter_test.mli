[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open DMotuzov_lib.Interpreter

(** Convert a runtime value to a human-readable string. *)
val string_of_value : value -> string

(** Pretty-print the whole environment. *)
val print_env : value Map.M(String).t -> unit

(** Parse and evaluate input program, printing results.
    @param input — code to run
    @param maxsteps — maximum evaluation steps *)
val run : string -> int -> unit
