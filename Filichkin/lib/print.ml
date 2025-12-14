[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let rec print_ast = function
  | Int n -> Printf.sprintf "Int %d" n
  | Var x -> Printf.sprintf "Var \"%s\"" x
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
    Printf.sprintf "Let (NonRec, \"%s\", %s, None)" x (print_ast e)
  | Let (NonRec, x, e, Some b) ->
    Printf.sprintf "Let (NonRec, \"%s\", %s, Some %s)" x (print_ast e) (print_ast b)
  | Let (Rec, x, e, None) -> Printf.sprintf "Let (Rec, \"%s\", %s, None)" x (print_ast e)
  | Let (Rec, x, e, Some b) ->
    Printf.sprintf "Let (Rec, \"%s\", %s, Some %s)" x (print_ast e) (print_ast b)
  | Abs (param, body) ->
    let param_str =
      match param with
      | Var x -> x
      | _ -> failwith "Abs parameter must be a variable"
    in
    Printf.sprintf "Abs (Var \"%s\", %s)" param_str (print_ast body)
  | App (f, a) -> Printf.sprintf "App (%s, %s)" (print_ast f) (print_ast a)
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
    Printf.sprintf "(%s %s %s)" (print_expr left) oper (print_expr right)
  | If (cond, thn, els) ->
    let else_branch =
      match els with
      | None -> ""
      | Some else_branch -> Printf.sprintf " else (%s)" (print_expr else_branch)
    in
    Printf.sprintf "if (%s) then (%s%s)" (print_expr cond) (print_expr thn) else_branch
  | Let (rec_f, name, value, body) ->
    let rec_branch =
      match rec_f with
      | Rec -> "rec "
      | NonRec -> ""
    in
    let body_branch =
      match body with
      | None -> ""
      | Some body_branch -> Printf.sprintf "in %s" (print_expr body_branch)
    in
    Printf.sprintf "let %s%s = (%s%s)" rec_branch name (print_expr value) body_branch
  | Abs (params, body) ->
    let param_str =
      match params with
      | Ast.Var s -> s
      | _ -> failwith "Abs parameter must be a variable"
    in
    Printf.sprintf "(fun (%s) -> (%s))" param_str (print_expr body)
  | App (func, arg) -> Printf.sprintf "(%s %s)" (print_expr func) (print_expr arg)
;;
