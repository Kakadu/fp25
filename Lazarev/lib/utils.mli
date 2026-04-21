[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module type READER_MONAD = sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val read : ('s, 's) t
  val run : ('s, 'a) t -> 's -> 'a
end

module type WRITER_MONAD = sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val write : 's -> ('s, unit) t
  val run : ('s, 'a) t -> 's list * 'a
end

module type STATE_MONAD = sig
  include READER_MONAD

  val write : 's -> ('s, unit) t
  val run : ('st, 'a) t -> 'st -> 'st * 'a
end
