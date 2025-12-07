[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Ast

let rec pp fmt = function
  | Num n -> fprintf fmt "%d" n
  | Var v -> fprintf fmt "%s" v
  | Unop (Inc, e) -> fprintf fmt "++%a" pp e
  | Unop (Dec, e) -> fprintf fmt "--%a" pp e
  | Binop (op, left, right) ->
    let op_str =
      match op with
      | Plus -> "+"
      | Minus -> "-"
      | Mult -> "*"
      | Div -> "/"
    in
    fprintf fmt "%a %s %a" pp left op_str pp right
  | If (cond, then_e, else_e) ->
    fprintf fmt "if %a then %a" pp cond pp then_e;
    (match else_e with
     | Some e -> fprintf fmt " else %a" pp e
     | None -> ())
  | Fun (name, e) -> fprintf fmt "fun %s -> %a" name pp e
  | Let (name, rhs, body) -> fprintf fmt "let %s = %a in %a" name pp rhs pp body
  | Letrec (name, rhs, body) -> fprintf fmt "let rec %s = %a in %a" name pp rhs pp body
  | App (f, x) -> fprintf fmt "%a %a" pp f pp x
  | Print e -> fprintf fmt "print %a" pp e
;;
