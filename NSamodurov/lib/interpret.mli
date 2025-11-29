[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type flags = { print_instr_exec : bool (** If set, print instruction executed *) }

val parse_and_run : string -> flags -> unit
