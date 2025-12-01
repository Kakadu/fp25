[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type varname = string

type unop =
  | Inc
  | Dec

type binop =
  | Plus
  | Minus
  | Mult
  | Div

(** The main type for AST *)
type expr =
  | Unit
  | Num of int (** integer constant *)
  | Var of varname (** variable *)
  | Unop of unop * expr (** unary operation *)
  | Binop of binop * expr * expr (** binary operation *)
  | If of expr * expr * expr option (** if-then-else *)
  | Fun of varname * expr (** function definitions *)
  | Let of varname * expr * expr (** local naming [let varname = expr in expr] *)
  | Letrec of varname * expr * expr
  (** recursive local naming [letrec varname = expr in expr] *)
  | Fix of expr (** fix point *)
  | App of expr * expr (** function applications *)
  | Print of expr (** integer printer *)
