(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val pp_expr : Format.formatter -> expr -> unit
val string_of_expr : expr -> string
