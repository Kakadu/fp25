[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and Yackovlev Nickolay *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string

type binop =
  | Add (** e1 + e2 *)
  | Sub (** e1 - e2 *)
  | Mul (** e1 * e2 *)
  | Div (** e1 / e2 *)

type cmpop =
  | Eq (** e1 = e2 *)
  | Neq (** e1 <> e2 *)
  | Lt (** e1 < e2 *)
  | Le (** e1 <= e2 *)
  | Gt (** e1 > e2 *)
  | Ge (** e1 >= e2 *)

type expr =
  | Var of name (** Variable, like x *)
  | Int of int (** Integer literal *)
  | Abs of name * expr (** Function: fun x -> e *)
  | App of expr * expr (** Function application: e1 e2 *)
  | Let of name * expr * expr (** Non-recursive let: let x = e1 in e2 *)
  | Let_rec of name * expr * expr (** Recursive let: let rec f = e1 in e2 *)
  | If of expr * expr * expr (** If expression *)
  | Binop of binop * expr * expr (** Arithmetic operation *)
  | Cmp of cmpop * expr * expr (** Comparison *)

type program = expr
