[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  [ `NotAValue of string
  | `Type_error of string
  | `Unbound
  | `Division_by_zero
  ]

val pp_error : Format.formatter -> [< error ] -> unit

module Interpret (M : Utils.MONAD_FAIL) : sig
  val run : int -> 'name Ast.t -> (int, [> error ]) M.t
end

val parse_and_run : ?max_steps:int -> string -> unit
