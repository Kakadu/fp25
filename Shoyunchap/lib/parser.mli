(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Public interface for the parser. *)

type error = [ `Parsing_error of string ]

val parse : string -> (Ast.expression, error) result
val pp_error : Format.formatter -> error -> unit
