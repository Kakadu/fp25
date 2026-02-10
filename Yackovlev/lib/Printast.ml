[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Ast

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
;;

let string_of_cmpop = function
  | Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
;;

let rec pp fmt = function
  | Var x -> pp_print_string fmt x
  | Int n when n < 0 -> fprintf fmt "(%d)" n
  | Int n -> fprintf fmt "%d" n
  | Abs (x, e) -> fprintf fmt "(fun %s -> %a)" x pp e
  | App (e1, e2) -> fprintf fmt "(%a %a)" pp e1 pp e2
  | Let (x, e1, e2) -> fprintf fmt "(let %s = %a in %a)" x pp e1 pp e2
  | Let_rec (f, e1, e2) -> fprintf fmt "(let rec %s = %a in %a)" f pp e1 pp e2
  | If (cond, t, e) -> fprintf fmt "(if %a then %a else %a)" pp cond pp t pp e
  | Binop (op, e1, e2) -> fprintf fmt "(%a %s %a)" pp e1 (string_of_binop op) pp e2
  | Cmp (op, e1, e2) -> fprintf fmt "(%a %s %a)" pp e1 (string_of_cmpop op) pp e2
;;

let pp_named = pp
let show ast = asprintf "%a" pp ast
