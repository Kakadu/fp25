[@@@ocaml.text "/*"]

(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type 'name t = 'name Ast.t =
  | Var of 'name
  | Abs of 'name * 'name t
  | App of 'name t * 'name t
  | Int of int
  | Let of 'name * 'name t * 'name t
  | Let_rec of 'name * 'name * 'name t * 'name t
  | If of 'name t * 'name t * 'name t
  | Binop of Ast.binop * 'name t * 'name t
  | Fix of 'name t

[@@deriving show { with_path = false}]

let pp_named = pp Format.pp_print_string