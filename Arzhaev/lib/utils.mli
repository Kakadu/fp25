[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitry Arzhaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module type TABLE = sig
  type 'a t

  val empty : 'a t
  val extend : string -> 'a -> 'a t -> 'a t
  val lookup : string -> 'a t -> 'a option
  val contains_value : 'a -> 'a t -> bool
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Table : TABLE

val charlst_to_str : char list -> string
val str_to_charlst : string -> char list
val get_next_letter : char -> char

module type SET_WITH_TO_LIST = sig
  include Set.S

  val to_list : t -> elt list
end

module SetWithToList : functor (S : Set.S) -> SET_WITH_TO_LIST with type elt = S.elt
