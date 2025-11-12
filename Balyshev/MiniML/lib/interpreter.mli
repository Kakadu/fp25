[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parsetree

type value =
  | VConstant of constant
  | VTuple of value * value * value list
  | VFun of pattern * expression * environment
  | VConstruct of string * value option

and error =
  | Is_not_a_function of expression
  | Unbound_value of string
  | Type_mismatch of string
  | Division_by_zero
  | Not_implemented of string

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

val show_value : value -> string
val pp_value : Format.formatter -> value -> unit
val show_error : error -> string
val pp_error : Format.formatter -> error -> unit

module Eval (_ : Monads.STATE_MONAD) : sig
  val eval_expr : Parsetree.expression -> (value, error) Result.t
end
