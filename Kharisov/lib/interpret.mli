[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  | Parse_error of string
  | Unbound_variable of string
  | Division_by_zero
  | Not_a_function
  | Type_error of string
  | Steps_exceeded

type state =
  { steps : int
  ; output : string list
  }

type 'a t = state -> ('a * state, error) result

type value =
  | VInt of int
  | VClosure of Ast.id * Ast.expr * env
  | VBuiltin of (value -> value t)

and env = (Ast.id * value) list

val return : 'a -> 'a t
val pp_value : Format.formatter -> value -> unit
val format_value : value -> string
val format_error : error -> string
val run : ?steps:int -> string -> (value * string list, error) result
