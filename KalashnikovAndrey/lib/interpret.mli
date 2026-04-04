[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type error =
  | Division_by_zero
  | Var_unbound of string
  | Out_of_steps
  | Type_error of string

val pp_error : Format.formatter -> error -> unit

module StateError : sig
  type 'a t = int -> ('a, error) result * int

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val step : unit t
  val fail : error -> 'a t
end

type value =
  | VInt of int
  | VClosure of string * expr * env
  | VBuiltin of (value -> value StateError.t)

and env = (string * value) list

val eval : env -> expr -> value StateError.t
val run_eval : string -> int -> bool -> unit
