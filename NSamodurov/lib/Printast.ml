[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type 'name t = 'name Ast.t =
  | EConst of Ast.const
  | EVar of 'name
  | ELet of Ast.let_flag * 'name * 'name t * 'name t
  | EIf of 'name t * 'name t * 'name t
  | EAbs of 'name * 'name t
  | EApp of 'name t * 'name t
[@@deriving show { with_path = false }]

let pp_named = pp Format.pp_print_string
