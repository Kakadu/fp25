[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Ast

let rec pp fmt = function
  | Num n -> fprintf fmt "%d" n
  | Var v -> fprintf fmt "%s" v
  | Binop (op, left, right) ->
    let op_str =
      match op with
      | Plus -> "+"
      | Minus -> "-"
      | Mult -> "*"
      | Div -> "/"
      | Equal -> "="
      | Less -> "<"
      | More -> ">"
    in
    fprintf fmt "%a %s %a" pp left op_str pp right
  | If (cond, then_e, else_e) ->
    fprintf fmt "if %a then %a else %a" pp cond pp then_e pp else_e
  | Fun (name, e) -> fprintf fmt "fun %s -> %a" name pp e
  | Let (name, rhs, body) -> fprintf fmt "let %s = %a in %a" name pp rhs pp body
  | Letrec (name, rhs, body) -> fprintf fmt "let rec %s = %a in %a" name pp rhs pp body
  | App (f, x) -> fprintf fmt "%a %a" pp f pp x
;;
