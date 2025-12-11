(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** Pretty-print expression *)
val pp_expr : Format.formatter -> expr -> unit

(** Convert expression to string *)
val string_of_expr : expr -> string

(** Print expression to standard output *)
val print_expr : expr -> unit

(** Print expression to channel *)
val print_expr_to_channel : out_channel -> expr -> unit