[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type typ =
  | TInt
  | TBool
  | TUnit
  | TFun of typ * typ
  | TTuple of typ list
  | TCon of string * typ list
  | TVar of tvar ref

and tvar =
  | Unbound of int
  | Link of typ

(* Полиморфная схема типов (нужна для сигнатуры infer) *)
type scheme = Forall of int list * typ

exception TypeError of string

type type_env =
  { vars : (string * scheme) list
  ; ctors : (string * scheme) list
  ; types : (string * string list) list
  ; type_def_ctors : (string * string list) list
  }

type tc_state

val string_of_type : typ -> string
val initial_env : type_env
val infer : type_env -> expr -> typ
val check_toplevel : tc_state -> toplevel -> (tc_state, ident) result
