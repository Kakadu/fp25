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

(** The main type for AST*)
type expr =
  | Const of num (** integer constant *)
  | Ident of ident (** identifier *)
  | Binexpr of binop * expr * expr (** binary arithmetic and comparisons *)
  | Ite of expr * expr * expr (** if then else *)
  | Abs of ident * expr (** abstraction of variable name and expression itself *)
  | App of app (** application *)
  | Let of bool * ident * expr * expr (** let and let rec bindings *)

and app =
  | Var of ident * expr (** apply bound with let function to expression *)
  | Fun of ident * expr * expr (** apply abstraction to expression *)
  | Application of app * expr (** apply application to expression *)
