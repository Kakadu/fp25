[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Type for interpreter's error *)
type error =
  | ExhaustedSteps (** When step limit is reached *)
  | ZeroDivision (** When divider has been evaluated to zero *)
  | InvalidApplication (** When application is not valid *)
  | InvalidLet (** When recursive let statement is used wrongly *)
  | UnboundVariable of Ast.name (** When provided name is not a variable *)
  | TypeMismatch of string * string (** When expected type differs from evaluated type *)

(** Type for evaluation result *)
type 'a result =
  | Eval of 'a
  | EvalError of error

(** Type for step limit *)
type step_limit =
  | Unlimited (** Count of steps is not bounded *)
  | Limited of int (** Count of steps is bounded and how many steps remaining *)

(** Type for interpreter's evaluated value *)
type value =
  | Unit
  | Int of int
  | Bool of bool
  | Tuple of value * value * value list
  | Closure of Ast.t * env
  | BuiltinAbstraction of (value -> value result)

(** Type for interpreter's enviropment *)
and env = (Ast.name * value) list * step_limit

(** Obtain empty enviropment with built-in functions *)
val empty_env : env

val show_value_type : value -> string
val show_value : value -> string
