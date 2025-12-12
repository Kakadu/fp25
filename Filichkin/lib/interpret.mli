[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(* let rec fix f eta = f ( fix f ) eta *)
type error =
  | UnboundVariable of string
  | DivisionByZero
  | TypeError of string
  | UnsupportedConstruct of string
  | IncorrectExpression
  | StackOverflow

type 'a eval_result = ('a, error) Result.t

type value =
  | VInt of int
  | VUnit
  | VClosure of string * expr * env ref
  | VBuiltin of (value -> value eval_result)

and env

val string_of_value : value -> string
val string_of_error : error -> string
val run_interpret : expr -> value eval_result
