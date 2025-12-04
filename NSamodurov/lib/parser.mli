[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Main entry of parser *)
type error = [ `ParsingError of string ] [@@deriving show { with_path = false }]

val parse : string -> (Ast.name Ast.t, [> `ParsingError of string ]) result
val to_brujin : string Ast.t -> Ast.brujin Ast.t

type dispatch =
  { expr : dispatch -> Ast.name Ast.t Angstrom.t
  ; apps : dispatch -> Ast.name Ast.t Angstrom.t
  ; single : dispatch -> Ast.name Ast.t Angstrom.t
  }

(* A collection of miniparsers *)
val parse_lam : dispatch
