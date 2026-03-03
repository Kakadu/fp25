[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Pretty-print representation of AST. *)

(** Verbose printing with optional compactness. Usable for parsing. *)
val pp : ?compact:bool -> Format.formatter -> Ast.expr -> unit

(** Print in fancy human-readable compact form with lambda notation. *)
val pp_hum : Format.formatter -> Ast.expr -> unit

(** Verbose printing without compact notation. *)
val pp_verbose : Format.formatter -> Ast.expr -> unit

(** Convert AST to string with optional compactness. *)
val show : ?compact:bool -> Ast.expr -> string

(** Extract free variables from an expression. *)
val free_vars : Ast.expr -> string list

(** Check if a variable is free in an expression. *)
val is_free_in : string -> Ast.expr -> bool
