[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Type for interpreter's error *)
type error =
  | InvalidApplication (** When application is not valid *)
  | InvalidLet (** When recursive let statement is used wrongly *)
  | UnboundVariable of Ast.name (** When provided name is not a variable *)
  | TypeMismatch of string (** When type is invalid *)
  | TypesMismatch of string * string (** When pair of types is invalid *)

(** Type for evaluation result *)
type 'a result =
  | Eval of 'a
  | EvalRaise of string
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
  | Exception of string
  | Closure of Ast.t * env
  | BuiltinAbstraction of (value -> value result)

(** Type for interpreter's enviropment *)
and env = (Ast.name * value) list * step_limit

(** Returns empty enviropment with built-in functions *)
val new_env : env

(** Returns empty enviropment with steps limit and built-in functions *)
val new_env_limited : int -> env

(** Runs the interpreter *)
val run : env -> Ast.t -> env * value result

(** Shows result in human-readable format *)
val show_result : value result -> string

(** Shows error in human-readable format *)
val show_error : error -> string
