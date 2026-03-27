[@@@ocaml.text "*/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "*/*"]

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONAD_ERROR = sig
  type ('e, 'a) t

  val return : 'a -> ('e, 'a) t
  val bind : ('e, 'a) t -> ('a -> ('e, 'b) t) -> ('e, 'b) t
  val fail : 'e -> ('e, 'a) t
  val catch : ('e, 'a) t -> ('e -> ('e, 'a) t) -> ('e, 'a) t
end

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
  include WRITER_MONAD with type ('s, 'a) t := ('s, 'a) t

  val run : ('st, 'a) t -> 'st -> 'st * 'a
end
