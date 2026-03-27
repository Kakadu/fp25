[@@@ocaml.text "*/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "*/*"]

(** Type for parsing error *)
type error =
  | UnexpectedEnd (** When end of source unexpectedly reached *)
  | UnexpectedRest of string (** When not all characters where parsed *)
  | SyntaxError of string (** For other errors *)

(** Type for result of parsing *)
type 'a result =
  | Parsed of 'a * char list
  | ParseError of error

(** Shows error in human readable format *)
val show_error : error -> string

(** Shows result in human readable format *)
val show_result : Ast.t result -> string

(** Parse expression *)
val parse_one : string -> Ast.t result
