(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error = [ `Parsing_error of string ]

val parse : string -> (Ast.expr, [> error ]) result
