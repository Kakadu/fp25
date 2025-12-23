[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let rec print_ast = function
  | Int n -> Printf.sprintf "Int %d" n
  | Var x -> Printf.sprintf "Var %S" x
  | BinOp (op, l, r) ->
    let op_str =
      match op with
      | Plus -> "Plus"
      | Minus -> "Minus"
      | Mult -> "Mult"
      | Div -> "Div"
      | Equal -> "Equal"
      | More -> "More"
      | Less -> "Less"
      | ELess -> "ELess"
      | EMore -> "EMore"
    in
    Printf.sprintf "BinOp (%s, %s, %s)" op_str (print_ast l) (print_ast r)
  | If (c, t, None) -> Printf.sprintf "If (%s, %s, None)" (print_ast c) (print_ast t)
  | If (c, t, Some e) ->
    Printf.sprintf "If (%s, %s, Some %s)" (print_ast c) (print_ast t) (print_ast e)
  | Let (NonRec, x, e, None) ->
    Printf.sprintf "Let (NonRec, %S, %s, None)" x (print_ast e)
  | Let (NonRec, x, e, Some b) ->
    Printf.sprintf "Let (NonRec, %S, %s, Some %s)" x (print_ast e) (print_ast b)
  | Let (Rec, x, e, None) -> Printf.sprintf "Let (Rec, %S, %s, None)" x (print_ast e)
  | Let (Rec, x, e, Some b) ->
    Printf.sprintf "Let (Rec, %S, %s, Some %s)" x (print_ast e) (print_ast b)
  | Abs (param, body) -> Printf.sprintf "Abs (%S, %s)" param (print_ast body)
  | App (f, a) -> Printf.sprintf "App (%s, %s)" (print_ast f) (print_ast a)
  | UnOp (_, e) -> Printf.sprintf "UnOp (%s, %s)" "-" (print_ast e)
;;

let rec print_expr = function
  | Int n -> string_of_int n
  | Var s -> s
  | BinOp (op, left, right) ->
    let oper =
      match op with
      | Plus -> "+"
      | Minus -> "-"
      | Mult -> "*"
      | Div -> "/"
      | Equal -> "="
      | More -> ">"
      | Less -> "<"
      | EMore -> ">="
      | ELess -> "<="
    in
    let left_str = print_expr left in
    let right_str = print_expr right in
    Printf.sprintf "((%s) %s (%s))" left_str oper right_str
  | UnOp (_, e) -> Printf.sprintf "(%s(%s))" "-" (print_expr e)
  | If (cond, thn, els) ->
    let cond_str = print_expr cond in
    let thn_str = print_expr thn in
    let els_str =
      match els with
      | None -> ""
      | Some e -> Printf.sprintf " else %s" (print_expr e)
    in
    Printf.sprintf "(if %s then %s%s)" cond_str thn_str els_str
  | Let (rec_f, name, value, body) ->
    let rec_prefix =
      match rec_f with
      | Rec -> "rec "
      | NonRec -> ""
    in
    let value_str = print_expr value in
    let body_str =
      match body with
      | None -> ""
      | Some b -> Printf.sprintf " in %s" (print_expr b)
    in
    Printf.sprintf "(let %s%s = %s%s)" rec_prefix name value_str body_str
  | Abs (param, body) -> Printf.sprintf "(fun %s -> %s)" param (print_expr body)
  | App (func, arg) -> Printf.sprintf "(%s %s)" (print_expr func) (print_expr arg)
;;
