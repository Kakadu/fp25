(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val print_expr : Ast.expression -> string
val string_of_expr : Ast.expression -> string
val print_value : Interpret.value -> string
val print_error : Interpret.eval_error -> string
val show_parse_error : Parser.error -> string
