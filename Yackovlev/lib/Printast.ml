[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

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

let rec pp pp_name fmt = function
  | Var x -> pp_name fmt x
  | Int n ->
      if n < 0 then fprintf fmt "(%d)" n
      else fprintf fmt "%d" n
  | Abs (x, e) -> fprintf fmt "(fun %a -> %a)" pp_name x (pp pp_name) e
  | App (e1, e2) -> fprintf fmt "(%a %a)" (pp pp_name) e1 (pp pp_name) e2
  | Let (x, e1, e2) ->
    fprintf fmt "(let %a = %a in %a)" pp_name x (pp pp_name) e1 (pp pp_name) e2
  | Let_rec (f, e1, e2) ->
    fprintf fmt "(let rec %a = %a in %a)" pp_name f (pp pp_name) e1 (pp pp_name) e2
  | If (cond, t, e) ->
    fprintf fmt "(if %a then %a else %a)" (pp pp_name) cond (pp pp_name) t (pp pp_name) e
  | Binop (op, e1, e2) ->
    fprintf fmt "(%a %s %a)" (pp pp_name) e1 (string_of_binop op) (pp pp_name) e2
  | Cmp (op, e1, e2) ->
    fprintf fmt "(%a %s %a)" (pp pp_name) e1 (string_of_cmpop op) (pp pp_name) e2
;;

let pp_named fmt ast = pp pp_print_string fmt ast
let show pp_name ast = asprintf "%a" (pp pp_name) ast