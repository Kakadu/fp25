[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type uop = Ast.uop =
  | Pos
  | Neg
[@@deriving show { with_path = false }]

type bop = Ast.bop =
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
[@@deriving show { with_path = false }]

type 'name t = 'name Ast.t =
  | Int of int
  | Var of 'name
  | Abs of 'name * 'name t
  | App of 'name t * 'name t
  | Binop of bop * 'name t * 'name t
  | Unop of uop * 'name t
[@@deriving show { with_path = false }]

let pp_named = pp Format.pp_print_string
