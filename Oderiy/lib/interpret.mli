[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  [ `UnknownVariable of string
  | `Type_error of string
  | `Division_by_zero
  | `Steps_exceeded
  ]

val pp_error : Format.formatter -> [< error ] -> unit
val run : int -> string Ast.t -> (int, [> error ]) result
val parse_and_run : ?max_steps:int -> string -> unit
