[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** AST type *)
type 'a t =
  | Integer of int (** Integer [a]*)
  | Var of 'a (** Variable: string, De brujin index [x] *)
  (* | Let of 'a * 'a t * 'a t (\** Let expression [let x = e1 in e2]*\) *)
  | Abs of 'a * 'a t (** Abstraction [λx.e] *)
  | App of 'a t * 'a t (** Abstraction [N M] *)

(** Type used for de Brujin nameless notation *)
type brujin =
  | Index of int
  | Blank

(** Type used for standard notation *)
type name = string
