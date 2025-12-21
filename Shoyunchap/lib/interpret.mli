(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Public interface for the miniML interpreter. *)
open Ast

type value =
  | VInt of int
  | VUnit
  | VClosure of name * expression * env
  | VBuiltin of (value -> value eval)

and env = (name * value ref) list

and eval_error =
  | Unbound_variable of name
  | Not_a_function of value
  | Not_an_int of value
  | Division_by_zero
  | Step_limit_exceeded
  | Fix_argument_shape

and 'a eval = int -> ('a * int, eval_error) result

module type MONAD = sig
  type 'a t = 'a eval

  val ok : 'a -> 'a t
  val fail : eval_error -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module EvalM : MONAD

val return : 'a -> 'a eval
val error : eval_error -> 'a eval
val ( let* ) : 'a eval -> ('a -> 'b eval) -> 'b eval
val step : unit eval
val lookup : env -> name -> value eval
val extend : env -> name -> value -> env
val apply : value -> value -> value eval
val eval : env -> expression -> value eval
val builtin_fix : value
val builtin_print_int : value
val builtin_print_newline : value
val initial_env : env
val run : ?max_steps:int -> expression -> (value, eval_error) result
val string_of_value : value -> string
val string_of_error : eval_error -> string
val max_steps_from_env : int -> int
val parse_and_run : string -> unit
