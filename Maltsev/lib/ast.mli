[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** integer alias *)
type num = int

(** string alias *)
type ident = string

type binop =
  | Plus (** + *)
  | Minus (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Eq (** = *)
  | Neq (** != *)
  | Le (** < *)
  | Bi (** > *)

type expr =
  | Const of num
  | Ident of ident
  | Binexpr of binop * expr * expr
  | Ite of expr * expr * expr
  | Abs of ident * expr
  | App of app
  | Let of bool * ident * expr * expr

and app =
  | Var of ident * expr
  | Fun of ident * expr * expr
  | Application of app * expr
