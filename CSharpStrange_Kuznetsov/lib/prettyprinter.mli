(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val pp_prog : Format.formatter -> program -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_stmt : Format.formatter -> stmt -> unit
val pp_field : Format.formatter -> field -> unit
val pp_ident : Format.formatter -> ident -> unit
val pp_type : Format.formatter -> _type -> unit
