(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** constants **)
let string_of_const = function
  | Int n -> string_of_int n
  | Unit -> "()"
;;

(** binary operations **)
let string_of_op = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpEq -> "="
  | OpGt -> ">"
  | OpLt -> "<"
  | OpGte -> ">="
  | OpLte -> "<="
;;

(** Gather arguments of nested fun x -> fun y -> ... *)
let rec collect_fun args = function
  | Fun (x, body) -> collect_fun (x :: args) body
  | e -> List.rev args, e
;;

let is_atomic = function
  | Const _ | Var _ -> true
  | _ -> false
;;

let paren_if cond s = if cond then "(" ^ s ^ ")" else s

let rec print_expr (e : expression) : string =
  match e with
  | Const c -> string_of_const c
  | Var name -> name
  | BinOp (op, l, r) ->
    Printf.sprintf "(%s %s %s)" (print_expr l) (string_of_op op) (print_expr r)
  | If (cond, thn, None) ->
    Printf.sprintf "(if %s then %s)" (print_expr cond) (print_expr thn)
  | If (cond, thn, Some els) ->
    Printf.sprintf
      "(if %s then %s else %s)"
      (print_expr cond)
      (print_expr thn)
      (print_expr els)
  | Let (_scope, kind, name, bound, None) ->
    let kwd =
      match kind with
      | NonRec -> "let"
      | Rec -> "let rec"
    in
    Printf.sprintf "(%s %s = %s)" kwd name (print_expr bound)
  | Let (_scope, kind, name, bound, Some body) ->
    let kwd =
      match kind with
      | NonRec -> "let"
      | Rec -> "let rec"
    in
    Printf.sprintf "(%s %s = %s in %s)" kwd name (print_expr bound) (print_expr body)
  | Fun (x, body) ->
    let args, core = collect_fun [ x ] body in
    let args_s = String.concat " " args in
    Printf.sprintf "(fun %s -> %s)" args_s (print_expr core)
  | App (f, arg) ->
    let head = paren_if (not (is_atomic f)) (print_expr f) in
    let arg_s =
      match arg with
      | Const _ | Var _ -> print_expr arg
      | _ -> "(" ^ print_expr arg ^ ")"
    in
    head ^ " " ^ arg_s
;;

let string_of_expr = print_expr
let print_value = Interpret.string_of_value
let print_error = Interpret.string_of_error
let show_parse_error e = Format.asprintf "%a" Parser.pp_error e
