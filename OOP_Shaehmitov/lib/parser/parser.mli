[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val parse : string -> (Ast.expr, string) result
val parse_structure_items : string -> (Ast.structure_item list, string) result
