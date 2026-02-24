[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Pretty printer for miniML AST *)

(** Print AST to formatter *)
val pp : Format.formatter -> string Ast.t -> unit

(** Convert AST to string *)
val to_string : string Ast.t -> string
