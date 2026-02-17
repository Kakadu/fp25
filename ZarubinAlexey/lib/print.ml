[@@@ocaml.text "/*"]

(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type 'name t = 'name Ast.t =
  | Var of 'name
  | Abs of 'name * 'name t
  | App of 'name t * 'name t
  | Int of int
  | Let of 'name * 'name t * 'name t
  | Let_rec of 'name * 'name * 'name t * 'name t
  | If of 'name t * 'name t * 'name t
  | Binop of binop * 'name t * 'name t
  | Fix of 'name t
[@@deriving show { with_path = false }]

let pp_named = pp Format.pp_print_string

(* Печать AST по именованным переменным *)
let print_ast (e : Ast.name Ast.t) : string = Format.asprintf "%a" pp_named e
