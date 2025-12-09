[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let pp_binop fmt = function
  | Add -> Format.fprintf fmt "+"
  | Mul -> Format.fprintf fmt "*"
  | Sub -> Format.fprintf fmt "-"
  | Div -> Format.fprintf fmt "/"
  | Leq -> Format.fprintf fmt "<="
;;

type 'name t = 'name Ast.t =
  | Var of 'name (** Variable [x] *)
  | Fun of 'name * 'name t
  | App of 'name t * 'name t
  | Int of int
  | Neg of 'name t
  | Bin of binop * 'name t * 'name t
  | Let of 'name * 'name t * 'name t
  | If of 'name t * 'name t * 'name t
  | LetRec of 'name * 'name t * 'name t
[@@deriving show { with_path = false }]

let pp_named = pp Format.pp_print_string
