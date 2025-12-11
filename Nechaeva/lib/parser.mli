(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error = [ `Parsing_error of string ]

(** Main entry of parser *)
val parse : string -> (Ast.expr, [> error ]) result
