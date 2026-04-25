[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitry Arzhaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Utils

(** Ground (base) types of the language. *)
type ground =
  | GInt (** Integer type *)
  | GBool (** Boolean type *)
  | GFloat (** Floating-point type *)

(** Types of the language. *)
type typ =
  | TGround of ground (** Ground (primitive) type *)
  | TVar of string (** Type variable *)
  | TArrow of typ * typ (** Function type [t1 -> t2] *)

(** Type scheme used in generalization (Hindley–Milner style). *)
type scheme =
  { vars : string list (** Universally quantified type variables *)
  ; ty : typ (** Monomorphic type *)
  }

(** Substitution mapping type variables to types. *)
type subst = typ Table.t

(** Typing state used during inference. *)
type state =
  { sub : subst (** Current substitution *)
  ; fresh : typ (** Fresh type generator state *)
  }

(** Errors that may occur during type inference. *)
type infer_error =
  | IUnboundValue of string (** Unbound variable in typing environment *)
  | IOccursCheck of string * typ (** Occurs check failure (infinite type detected) *)
  | ITypeMismatch of typ * typ (** Type mismatch between two types *)
  | ITypeError (** Generic type inference error *)

(** Pretty-printer for type inference errors. *)
val pp_infer_error : Format.formatter -> infer_error -> unit

(** Result of type inference:
    either a failure or a successful result with updated state. *)
type 'a infresult =
  | IFailed of infer_error
  | IOk of state * 'a

(** Result of type-checking a toplevel phrase. *)
type toplevel_result =
  | RLet of string * scheme (** Type of a [let x = e] binding *)
  | RExpr of typ (** Type of an expression *)

(** Pretty-printer for types. *)
val pp_typ : Format.formatter -> typ -> unit

(** Pretty-printer for type schemes. *)
val pp_scheme : Format.formatter -> scheme -> unit

(** Pretty-printer for toplevel inference results. *)
val pp_toplevel_result : Format.formatter -> toplevel_result -> unit

(** Run type inference on a toplevel expression.

    @param toplevel expression to type-check
    @param env typing environment mapping variables to schemes
    @return updated environment and inferred type result *)
val run_infer
  :  toplevel
  -> scheme Utils.Table.t
  -> (scheme Utils.Table.t * toplevel_result) infresult
