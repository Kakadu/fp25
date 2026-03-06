(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common

val typecheck : c_sharp_class -> TypeCheck.state * (unit, error) result
val typecheck_main : c_sharp_class -> Ast.ident option * (unit, Common.error) result
