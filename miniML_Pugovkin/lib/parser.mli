[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

exception Error of string

val expr_of_string : string -> expr
val toplevel_of_string : string -> toplevel
val program_of_string : string -> program
