[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type error =
  | UnboundVariable of string
  | DivisionByZero
  | TypeError of string
  | UnsupportedConstruct of string
  | IncorrectExpression
  | StepCountIsZero

type 'a eval_result = ('a, error) Result.t

type env = (string * value) list

and value =
  | VInt of int
  | VUnit
  | VBool of bool
  | VTuple of value list
  | VClosure of pattern * expr * env
  | VBuiltin of (value -> value eval_result)
  | VConstr of string * value list

type state

val initial_state : state
val string_of_value : value -> string
val string_of_error : error -> string
val interpret_toplevel : state -> toplevel -> (state * value option, error) result
val interpret_program : state -> Ast.toplevel list -> (state * value option) eval_result
val run_interpret : Ast.expr -> value eval_result
