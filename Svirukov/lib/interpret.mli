[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

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

and env = (string, value, Base.String.comparator_witness) Base.Map.t

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

val run_interpret : expr -> int -> (expr, error) result
