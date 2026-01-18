[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Error : sig
  type t =
    | Not_implemented of string
    | Occurs_check of string * Typedtree.ty
    | Unification_failed of Typedtree.ty * Typedtree.ty
    | Type_was_not_declared of string
    | Unbound_value of string
    | Unbound_type_variable of string
    | Unbound_constructor of string
    | Type_param_duplicates of string
    | Constructor_arity_mismatch of string
    | Left_of_let_rec
    | Right_of_let_rec
    | Constructor_name_duplicates of string
    | TypeEnv_invariant_violation of string

  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module Make (_ : Monads.STATE_MONAD) : sig
  val infer_expression : Parsetree.expression -> (Typedtree.ty, Error.t) result
  val infer_structure : Parsetree.structure -> (Typedtree.structure, Error.t) result
end
