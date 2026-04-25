[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitry Arzhaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(** Errors that may occur during parsing. *)
type parse_error =
  | PExpectedInt (** Expected an integer literal. *)
  | PExpectedFloat (** Expected a floating-point literal. *)
  | PExpectedId (** Expected an identifier. *)
  | PSyntaxError (** Generic syntax error. *)

(** Pretty-printer for parse errors. *)
val pp_parse_error : Format.formatter -> parse_error -> unit

(** Parser input representation: a list of characters. *)
type input = char list

(** Result of parsing.
    Either a failure with a [parse_error], or a successfully parsed value
    together with the remaining unconsumed input. *)
type 'a parse_result =
  | PFailed of parse_error
  | Parsed of 'a * input

(** A parser consumes input and produces a parse result. *)
type 'a parser = input -> 'a parse_result

(** Top-level parser function.

    Parses a string into a [toplevel] abstract syntax tree.

    @param input source code
    @return parsed AST or a parse error *)
val parser : string -> toplevel parse_result
