[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type value =
  | VConst of const
  | VTuple of value * value * value list
  | VFun of rec_flag * pattern * expression * environment
  | VOption of value option
  | VPrintInt

and error =
  [ `Is_not_a_function of string
  | `Unbound_value of string
  | `Type_mismatch of expression
  | `Division_by_zero
  ]

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

val init_env : (string, value, Base.String.comparator_witness) Base.Map.t
val eval_expr : expression -> (value, error) result
val show_value : value -> string
val show_error : error -> string
val pp_value : Format.formatter -> value -> unit
val pp_error : Format.formatter -> error -> unit
