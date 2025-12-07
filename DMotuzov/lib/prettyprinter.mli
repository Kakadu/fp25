[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(** Pretty-print expression *)
val pp : Format.formatter -> expression -> unit

(** Pretty-print top-level definition *)
val pp_top_let : Format.formatter -> toplevel -> unit

(** Pretty-print a whole program (structure) *)
val pp_prog : Format.formatter -> structure -> unit
