[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val parse_expression : string -> (Parsetree.expression, string) Result.t
val parse_structure : string -> (Parsetree.structure, string) Result.t

(* testing stuff *)
val parse_pattern : string -> (Parsetree.pattern, string) Result.t
val parse_core_type : string -> (Parsetree.core_type, string) Result.t
