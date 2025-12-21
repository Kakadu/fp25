(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | UnboundVariable of string
  | TypeError of string
  | DivisionByZero
  | NotAFunction
  | ExceedMaxSteps

type value =
  | VInt of int
  | VUnit
  | VClosure of string list * expr * (string * value) list
  | VBuiltin of (value -> (value, error) result)

type env = (string * value) list

module StepErrorM : sig
  type 'a t = int -> int * ('a, error) result

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : error -> 'a t
  val get_steps : int t
  val dec_steps : unit t
  val run : 'a t -> int -> int * ('a, error) result
end

val env_empty : env
val env_extend : env -> string -> value -> env
val env_lookup : env -> string -> (value, error) result
val initial_env : env

module Interpreter : sig
  val eval : expr -> env -> value StepErrorM.t
  val apply : value -> value -> value StepErrorM.t
end

val run_interpreter : expr -> int -> (value, error) result
