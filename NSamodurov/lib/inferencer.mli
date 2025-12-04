[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Scheme : sig
  type t

  val mono : Type.ty -> Type.scheme
end

module Subst : sig
  type t

  val empty : t
  val remove : int -> t -> t
  val apply : t -> Type.ty -> Type.ty
  val extend : t -> int -> Type.ty -> t
end

module InferMonad : sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val fail : 's -> ('s, 'a) t
  val run : ('s, 'a) t -> (Subst.t * 'a, 's) Result.t
  val fresh : ('s, int) t
  val subst : ('s, Subst.t) t
  val extend : int -> Type.ty -> ('s, unit) t

  module Syntax : sig
    val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
    val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  end

  val fail : 's -> ('s, 'a) t
  val run : ('s, 'a) t -> (Subst.t * 'a, 's) Result.t
end

type error =
  [ `OccursCheck of Type.ty * Type.ty
  | `UnifyError of Type.ty * Type.ty
  | `UnboundVariable of Ast.name * int
  ]
[@@deriving show { with_path = false }]

val w
  :  Ast.brujin Ast.t
  -> ( Type.ty
       , [ `OccursCheck of Type.ty * Type.ty
         | `UnifyError of Type.ty * Type.ty
         | `UnboundVariable of Ast.name * int
         ] )
       Result.t

val env : Type.scheme Type.IMap.t
