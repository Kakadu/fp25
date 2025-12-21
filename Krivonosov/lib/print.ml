[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(** Structural printing for debugging *)
let rec print_ast = function
  | Int n -> Printf.sprintf "Int %d" n
  | Var x -> Printf.sprintf "Var %S" x
  | BinOp (op, l, r) ->
    let op_str =
      match op with
      | Add -> "Add"
      | Sub -> "Sub"
      | Mul -> "Mul"
      | Div -> "Div"
      | Mod -> "Mod"
      | Eq -> "Eq"
      | Neq -> "Neq"
      | Lt -> "Lt"
      | Gt -> "Gt"
      | Leq -> "Leq"
      | Geq -> "Geq"
    in
    Printf.sprintf "BinOp (%s, %s, %s)" op_str (print_ast l) (print_ast r)
  | If (c, t, None) -> Printf.sprintf "If (%s, %s, None)" (print_ast c) (print_ast t)
  | If (c, t, Some e) ->
    Printf.sprintf "If (%s, %s, Some %s)" (print_ast c) (print_ast t) (print_ast e)
  | Let (is_rec, x, e, b) ->
    Printf.sprintf
      "Let (%s, %S, %s, %s)"
      (if is_rec then "true" else "false")
      x
      (print_ast e)
      (print_ast b)
  | Abs (param, body) -> Printf.sprintf "Abs (%S, %s)" param (print_ast body)
  | App (f, a) -> Printf.sprintf "App (%s, %s)" (print_ast f) (print_ast a)
;;

(** Human-readable printing *)
let rec print_expr = function
  | Int n -> string_of_int n
  | Var s -> s
  | BinOp (op, l, r) ->
    let op_str =
      match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Mod -> "%"
      | Eq -> "="
      | Neq -> "<>"
      | Lt -> "<"
      | Gt -> ">"
      | Leq -> "<="
      | Geq -> ">="
    in
    Printf.sprintf "(%s %s %s)" (print_expr l) op_str (print_expr r)
  | If (c, t, None) -> Printf.sprintf "(if %s then %s)" (print_expr c) (print_expr t)
  | If (c, t, Some e) ->
    Printf.sprintf "(if %s then %s else %s)" (print_expr c) (print_expr t) (print_expr e)
  | Let (is_rec, name, value, body) ->
    Printf.sprintf
      "(let %s%s = %s in %s)"
      (if is_rec then "rec " else "")
      name
      (print_expr value)
      (print_expr body)
  | Abs (param, body) -> Printf.sprintf "(fun %s -> %s)" param (print_expr body)
  | App (f, a) -> Printf.sprintf "(%s %s)" (print_expr f) (print_expr a)
;;
