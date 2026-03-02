[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Smart constructors *)

val int_cons : int -> 'a Ast.t
val var : 'a -> 'a Ast.t
val abs : 'a -> 'a Ast.t -> 'a Ast.t
val app : 'a Ast.t -> 'a Ast.t -> 'a Ast.t

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end

type output =
  | OUnit
  | OInt of int

type error =
  | UnknownVariable of string
  | TypeError of string
  | DivisionByZero
  | ProgramFreeze

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val fail : error -> 'a t
end

module RESULT : MONADERROR with type 'a t = ('a, error) Result.t
