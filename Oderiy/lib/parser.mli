[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error = [ `Parsing_error of string ]

val pp_error : Format.formatter -> [< `Parsing_error of string ] -> unit

(** Main entry of parser *)
val parse : string -> (Ast.name Ast.t, [> error ]) result

(** Parser combinators organized by precedence *)
type parsers =
  { p_expr : parsers -> Ast.name Ast.t Angstrom.t
  ; p_comp : parsers -> Ast.name Ast.t Angstrom.t
  ; p_add : parsers -> Ast.name Ast.t Angstrom.t
  ; p_mul : parsers -> Ast.name Ast.t Angstrom.t
  ; p_unary : parsers -> Ast.name Ast.t Angstrom.t
  ; p_app : parsers -> Ast.name Ast.t Angstrom.t
  ; p_primary : parsers -> Ast.name Ast.t Angstrom.t
  }

(* Parser collection *)
val parse_lam : parsers
