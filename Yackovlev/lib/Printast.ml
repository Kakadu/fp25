[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type binop = Ast.binop =
  | Add
  | Sub
  | Mul
  | Div
[@@deriving show { with_path = false }]

type cmpop = Ast.cmpop =
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
[@@deriving show { with_path = false }]

type 'name t = 'name Ast.t =
  | Var of 'name
  | Int of int
  | Abs of 'name * 'name t
  | App of 'name t * 'name t
  | Let of 'name * 'name t * 'name t
  | Let_rec of 'name * 'name t * 'name t
  | If of 'name t * 'name t * 'name t
  | Binop of binop * 'name t * 'name t
  | Cmp of cmpop * 'name t * 'name t
[@@deriving show { with_path = false }]

let pp_named = pp Format.pp_print_string
