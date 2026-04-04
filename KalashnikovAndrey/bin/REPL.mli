[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val prompt : string
val read_max_steps : unit -> int
val read_printer_flag : unit -> bool
val repl : int -> bool -> unit
