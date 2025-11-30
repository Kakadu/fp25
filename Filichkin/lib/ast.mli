[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Equal
  | More
  | Less
  | EMore
  | ELess

type rec_flag =
  | NonRec
  | Rec

type expr =
  | Int of int
  | Var of string
  | BinOp of binop * expr * expr
  | If of expr * expr * expr option (** if c then t [else e] *)
  | Let of rec_flag * string * expr * expr option
  | Abs of string list * expr (** sugar: fun x y -> ... *)
  | App of expr * expr
  | Seq of expr list
  | Fix of expr
