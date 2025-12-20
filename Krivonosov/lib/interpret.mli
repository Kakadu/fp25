[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Error types for interpreter *)
type error =
  [ `UnknownVariable of string
  | `DivisionByZero
  | `TypeMismatch
  | `StepLimitExceeded
  ]

(** Values in our interpreter *)
type value =
  | VInt of int
  | VClosure of string * string Ast.t * environment
  | VBuiltin of string * (value -> (value, error) Base.Result.t)
  | VUnit

and environment = (string * value) list

(** Evaluate an AST expression with optional step limit *)
val eval_expr : ?max_steps:int -> string Ast.t -> (value, error) Base.Result.t

(** Parse and run a string (for testing) *)
val parse_and_run : string -> unit
