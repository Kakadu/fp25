(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"

let string_of_compop = function
  | Equal -> "="
  | NotEqual -> "<>"
  | Less -> "<"
  | LessEq -> "<="
  | Greater -> ">"
  | GreaterEq -> ">="

let string_of_rec_flag = function
  | NonRecursive -> ""
  | Recursive -> "rec "

let rec pp_expr fmt = function
  | Const (Int n) -> Format.fprintf fmt "%d" n
  | Const Unit -> Format.fprintf fmt "()"
  | Var name -> Format.fprintf fmt "%s" name
  | Abs (params, body) ->
      Format.fprintf fmt "(fun %a -> %a)"
        (Format.pp_print_list 
          ~pp_sep:Format.pp_print_space 
          Format.pp_print_string) 
        params
        pp_expr body
  | App (func, arg) ->
      Format.fprintf fmt "(%a %a)" pp_expr func pp_expr arg
  | Let (flag, name, value, body) ->
      Format.fprintf fmt "(let %s%s = %a in %a)"
        (string_of_rec_flag flag)
        name
        pp_expr value
        pp_expr body
  | BinOp (op, left, right) ->
      Format.fprintf fmt "(%a %s %a)"
        pp_expr left
        (string_of_binop op)
        pp_expr right
  | Comp (op, left, right) ->
      Format.fprintf fmt "(%a %s %a)"
        pp_expr left
        (string_of_compop op)
        pp_expr right
  | If (cond, then_expr, else_expr) ->
      Format.fprintf fmt "(if %a then %a else %a)"
        pp_expr cond
        pp_expr then_expr
        pp_expr else_expr

let string_of_expr e = 
  Format.asprintf "%a" pp_expr e

let print_expr e = 
  Format.printf "%a\n" pp_expr e

let print_expr_to_channel chan e =
  Format.fprintf (Format.formatter_of_out_channel chan) "%a\n" pp_expr e