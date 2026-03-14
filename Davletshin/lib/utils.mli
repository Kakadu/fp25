[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(** Smart constructors *)

val int_cons : int -> 'a t
val var : 'a -> 'a t
val abs : 'a -> 'a t -> 'a t
val app : 'a t -> 'a t -> 'a t

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end

type output =
  | OUnit
  | OInt of int
  | OAbs of name * name t
  | OBuiltin of name

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
