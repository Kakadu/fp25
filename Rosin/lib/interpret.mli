[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type value =
  | VUnit
  | VNum of int
  | VClosure of varname * expr * env
  | VRecClosure of varname * varname * expr * env

and env = (varname * value) list

type error =
  | UnboundVariable of varname
  | DivisionByZero
  | StepLimitExceeded
  | NonFunctionApplication of value
  | NonIntegerCondition of value
  | InvalidUnop of unop * value
  | InvalidBinop of binop * value * value
  | TypeError
  | LetWithoutBody
  | LetrecWithoutBody

val run_interpret : expr -> int -> (value, error) result
