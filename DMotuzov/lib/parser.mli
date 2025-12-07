[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(** Parse a single identifier *)
val parse_identifier : string Angstrom.t

(** Parse integer constant *)
val parse_constant : expression Angstrom.t

(** Parse additive operator: + or - *)
val parse_additive_operator : binary_op Angstrom.t

(** Parse multiplicative operator: * or / *)
val parse_multiplicative_operator : binary_op Angstrom.t

(** Parse full expression *)
val parse_expression : expression Angstrom.t

(** Parse top-level definition: [let x = ... ;;] *)
val parse_toplevel_let : toplevel Angstrom.t

(** Parse top-level recursive definition: [let rec f = ... ;;] *)
val parse_toplevel_let_rec : toplevel Angstrom.t

(** Parse entire structure: sequence of toplevel definitions *)
val parse_structure : structure Angstrom.t

(** Run parser on input string *)
val parse : string -> (structure, string) result
