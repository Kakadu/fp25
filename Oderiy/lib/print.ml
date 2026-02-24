[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let rec pp fmt = function
  | Int n -> Format.fprintf fmt "%d" n
  | Var name -> Format.fprintf fmt "%s" name
  | Fun (param, body) -> Format.fprintf fmt "(fun %s -> %a)" param pp body
  | App (f, arg) -> Format.fprintf fmt "(%a %a)" pp f pp arg
  | Neg e -> Format.fprintf fmt "(-%a)" pp e
  | Bin (op, left, right) ->
    let op_str =
      match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Lt -> "<"
      | Leq -> "<="
      | Eq -> "="
      | Geq -> ">="
      | Gt -> ">"
    in
    Format.fprintf fmt "(%a %s %a)" pp left op_str pp right
  | Let (name, value, body) ->
    Format.fprintf fmt "(let %s = %a in %a)" name pp value pp body
  | LetRec (fname, func, body) ->
    Format.fprintf fmt "(let rec %s = %a in %a)" fname pp func pp body
  | If (cond, then_br, else_br) ->
    Format.fprintf fmt "(if %a then %a else %a)" pp cond pp then_br pp else_br
  | Fix -> Format.fprintf fmt "fix"
;;

let to_string expr = Format.asprintf "%a" pp expr
