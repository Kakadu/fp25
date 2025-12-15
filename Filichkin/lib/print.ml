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
  | Abs (param, body) ->
    let param_str =
      match param with
      | Var x -> x
      | _ -> "???"
    in
    Printf.sprintf "Abs (Var %S, %s)" param_str (print_ast body)
  | App (f, a) -> Printf.sprintf "App (%s, %s)" (print_ast f) (print_ast a)
;;

let precedence = function
  | Mult | Div -> 3
  | Plus | Minus -> 2
  | Equal | More | Less | EMore | ELess -> 1
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
    let need_parens_left =
      match left with
      | BinOp (inner_op, _, _) -> precedence inner_op < precedence op
      | If _ | Let _ | Abs _ | App _ -> true
      | _ -> false
    in
    let need_parens_right =
      match right with
      | BinOp (inner_op, _, _) -> precedence inner_op <= precedence op
      | If _ | Let _ | Abs _ | App _ -> true
      | _ -> false
    in
    let left_str =
      if need_parens_left then Printf.sprintf "(%s)" left_str else left_str
    in
    let right_str =
      if need_parens_right then Printf.sprintf "(%s)" right_str else right_str
    in
    Printf.sprintf "%s %s %s" left_str oper right_str
  | If (cond, thn, els) ->
    let cond_str = print_expr cond in
    let thn_str = print_expr thn in
    (* Add parentheses around nested if, let, Abs, or App in then branch when outer if has else *)
    let thn_str =
      match thn, els with
      | If _, Some _ | Let _, Some _ | Abs _, Some _ | App _, Some _ ->
        Printf.sprintf "(%s)" thn_str
      | _ -> thn_str
    in
    let else_str =
      match els with
      | None -> ""
      | Some e ->
        let e_str = print_expr e in
        (* Add parentheses around let without in in else branch to avoid ambiguity *)
        let e_str =
          match e with
          | Let (_, _, _, None) -> Printf.sprintf "(%s)" e_str
          | _ -> e_str
        in
        Printf.sprintf " else %s" e_str
    in
    Printf.sprintf "if %s then %s%s" cond_str thn_str else_str
  | Let (rec_f, name, value, body) ->
    let rec_prefix =
      match rec_f with
      | Rec -> "rec "
      | NonRec -> ""
    in
    let value_str = print_expr value in
    (* Add parentheses around nested let, if without else, or Abs in value when outer let has body *)
    let value_str =
      match value, body with
      | Let _, Some _ | If (_, _, None), Some _ | Abs _, Some _ ->
        Printf.sprintf "(%s)" value_str
      | _ -> value_str
    in
    let body_str =
      match body with
      | None -> ""
      | Some b -> Printf.sprintf " in %s" (print_expr b)
    in
    Printf.sprintf "let %s%s = %s%s" rec_prefix name value_str body_str
  | Abs (params, body) ->
    let param_str =
      match params with
      | Ast.Var s -> s
      | _ -> "???"
    in
    let body_str = print_expr body in
    (* Add parentheses around let without in or if without else when they appear in Abs body *)
    let body_str =
      match body with
      | Let (_, _, _, None) | If (_, _, None) -> Printf.sprintf "(%s)" body_str
      | _ -> body_str
    in
    Printf.sprintf "fun %s -> %s" param_str body_str
  | App (func, arg) ->
    let func_str = print_expr func in
    let arg_str = print_expr arg in
    let need_parens_func =
      match func with
      | Abs _ | Let _ | If _ | BinOp _ -> true
      | _ -> false
    in
    let need_parens_arg =
      match arg with
      | Abs _ | Let _ | If _ | App _ | BinOp _ -> true
      | _ -> false
    in
    let func_str =
      if need_parens_func then Printf.sprintf "(%s)" func_str else func_str
    in
    let arg_str = if need_parens_arg then Printf.sprintf "(%s)" arg_str else arg_str in
    Printf.sprintf "%s %s" func_str arg_str
;;
