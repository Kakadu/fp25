[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error = Parsing_error of string

val pp_error : Format.formatter -> error -> unit
val parse : string -> (Ast.expr, error) result
