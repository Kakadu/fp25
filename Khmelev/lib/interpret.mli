[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  [ `UnknownVariable of string
  | `TypeError of string
  | `DivisionByZero
  | `StepLimitReached
  ]

type value =
  | VInt of int
  | VClosure of Ast.name * Ast.expr * env
  | VBuiltin of (value -> (value, error) Result.t)

and env = (Ast.name * value ref) list

val pp_error : Format.formatter -> error -> unit
val string_of_value : value -> string
val initial_env : env
val eval : ?step_limit:int -> ?env:env -> unit -> Ast.expr -> (value, error) Result.t
val parse_and_run : ?step_limit:int -> string -> (string, error) Result.t
