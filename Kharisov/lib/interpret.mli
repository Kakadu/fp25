[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* error *)
type error =
  [ `Parse_error of string
  | `Unbound_variable of string
  | `Division_by_zero
  | `Not_a_function
  | `Type_error of string
  | `Steps_exceeded
  ]

module Env : Map.S with type key = Ast.id

type value =
  | VInt of int
  | VClosure of Ast.id * Ast.expr * env
  | VRecClosure of Ast.id * Ast.id * Ast.expr * env
  | VBuiltin of string

and env = value Env.t

val pp_value : Format.formatter -> value -> unit
val format_value : value -> string
val format_error : error -> string
val run : ?steps:int -> string -> (value * string list, [> error ]) result
