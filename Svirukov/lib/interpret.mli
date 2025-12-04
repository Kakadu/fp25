(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | UnboundVariable of string
  | TypeError of string
  | DivisionByZero
  | ParttialApplication
  | TooManyArgs
  | ExceedNumberOfSteps of expr
  | Unimplemented

type value =
  | VInt of int
  | VClosure of pattern * expr

type env = (string, value, Base.String.comparator_witness) Base.Map.t

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

val run_interpret : expr -> int -> (expr, error) result
