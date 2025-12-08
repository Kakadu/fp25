[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Name of variable or function *)
type name = string

(* Binary arithmetic operations *)
type binop =
  | Add
  | Mul
  | Sub
  | Div

(* Comparison operations *)
type cmpop =
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge

(* AST for miniML expressions *)
type 'name t =
  | Var of 'name
  | Int of int
  | Abs of 'name * 'name t
  | App of 'name t * 'name t
  | Let of 'name * 'name t * 'name t
  | Let_rec of 'name * 'name t * 'name t
  | If of 'name t * 'name t * 'name t
  | Binop of binop * 'name t * 'name t
  | Cmp of cmpop * 'name t * 'name t

(* Expression with string names *)
type expr = name t

(* Whole program is one expression *)
type program = expr
