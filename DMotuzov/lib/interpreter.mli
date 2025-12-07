[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type error =
  | TypeError
  | DivisionByZero
  | NoVariable of identificator
  | OutOfMaxSteps

module Res : sig
  type 'a t = ('a, error) result

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

type value =
  | ValInt of int
  | ValUnit
  | ValFun of identificator * expression * env
  | RecClosure of identificator * identificator * expression * env
  | Builtin of (value -> value Res.t)

and env = (identificator, value, Base.String.comparator_witness) Base.Map.t

module EvalEnv : sig
  val empty : env
  val extend : env -> identificator -> value -> env
  val find_expr : env -> identificator -> value Res.t
end

module Inter : sig
  (** Evaluate top-level binding *)
  val eval_top_let : env -> int -> toplevel -> (env * int, error) result

  (** Evaluate whole program *)
  val eval_program : env -> int -> structure -> (env * int, error) result
end

val empty_with_builtins : env
val run_interpreter : structure -> int -> (env, error) result
