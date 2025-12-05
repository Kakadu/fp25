(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val print_ast : Ast.expr -> string
val print_error : Interpret.error -> string
val print_typ : Inferencer.typ -> string
val show_type_error : Inferencer.type_error -> string
