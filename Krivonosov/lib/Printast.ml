[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type binop = Ast.binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
[@@deriving show { with_path = false }]

type 'name t = 'name Ast.t =
  | Var of 'name
  | Abs of 'name * 'name t
  | App of 'name t * 'name t
  | Int of int
  | BinOp of binop * 'name t * 'name t
  | If of 'name t * 'name t * 'name t option
  | Let of bool * 'name * 'name t * 'name t
[@@deriving show { with_path = false }]

let pp_named = pp Format.pp_print_string
