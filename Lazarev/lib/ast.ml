[@@@ocaml.text "*/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "*/*"]

type name =
  | Wildcard
  | Real of string

type unary_operation =
  | Neg
  | Not
[@@deriving show { with_path = false }]

type binary_operation =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
[@@deriving show { with_path = false }]

type let_mnemonic =
  | Let
  | LetRec
[@@deriving show { with_path = false }]

type t =
  | Unit
  | Int of int
  | Bool of bool
  | Var of name
  | Tuple of t * t * t list
  | UnaryOp of unary_operation * t
  | BinaryOp of binary_operation * t * t
  | IfThenElse of t * t * t
  | LetStatement of let_mnemonic * name * t
  | LetExpr of let_mnemonic * name * t * t
  | Abstraction of name * t
  | Application of t * t

let show_name = function
  | Wildcard -> "_"
  | Real name -> name
;;

let rec show_ast = function
  | Unit -> "unit"
  | Int value -> string_of_int value
  | Bool value -> string_of_bool value
  | Var name -> show_name name
  | Tuple (first, second, rest) ->
    Format.sprintf
      "Tuple(%s)"
      (String.concat ", " (List.map show_ast (first :: second :: rest)))
  | UnaryOp (op, expr) ->
    Format.sprintf "%s(%s)" (show_unary_operation op) (show_ast expr)
  | BinaryOp (op, left, right) ->
    Format.sprintf
      "%s(%s, %s)"
      (show_binary_operation op)
      (show_ast left)
      (show_ast right)
  | IfThenElse (cond, then_expr, else_expr) ->
    Format.sprintf
      "IfThenElse(%s, %s, %s)"
      (show_ast cond)
      (show_ast then_expr)
      (show_ast else_expr)
  | LetStatement (rf, name, statement) ->
    Format.sprintf
      "%s(%s, %s)"
      (show_let_mnemonic rf)
      (show_name name)
      (show_ast statement)
  | LetExpr (rf, name, lhs, rhs) ->
    Format.sprintf
      "%s(%s, %s, %s)"
      (show_let_mnemonic rf)
      (show_name name)
      (show_ast lhs)
      (show_ast rhs)
  | Abstraction (name, expr) ->
    Format.sprintf "Abs(%s, %s)" (show_name name) (show_ast expr)
  | Application (lhs, rhs) -> Format.sprintf "App(%s, %s)" (show_ast lhs) (show_ast rhs)
;;

let show_pretty_unary_operation = function
  | Neg -> "-"
  | Not -> "!"
;;

let show_pretty_binary_operation = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | And -> "&&"
  | Or -> "||"
  | Equal -> "="
  | NotEqual -> "<>"
  | Less -> "<"
  | LessEqual -> "<="
  | Greater -> ">"
  | GreaterEqual -> ">="
;;

let show_pretty_rec_flag = function
  | Let -> "let"
  | LetRec -> "let rec"
;;

let rec show_pretty_ast = function
  | Unit -> "()"
  | Int value -> string_of_int value
  | Bool value -> string_of_bool value
  | Var name -> show_name name
  | Tuple (first, second, rest) ->
    Format.sprintf
      "(%s)"
      (String.concat ", " (List.map show_pretty_ast (first :: second :: rest)))
  | UnaryOp (op, expr) ->
    Format.sprintf "%s(%s)" (show_pretty_unary_operation op) (show_pretty_ast expr)
  | BinaryOp (op, left, right) ->
    Format.sprintf
      "(%s %s %s)"
      (show_pretty_ast left)
      (show_pretty_binary_operation op)
      (show_pretty_ast right)
  | IfThenElse (cond, then_expr, else_expr) ->
    Format.sprintf
      "if (%s) then (%s) else (%s)"
      (show_pretty_ast cond)
      (show_pretty_ast then_expr)
      (show_pretty_ast else_expr)
  | LetStatement (rf, name, statement) ->
    Format.sprintf
      "%s %s = (%s)"
      (show_pretty_rec_flag rf)
      (show_name name)
      (show_pretty_ast statement)
  | LetExpr (rf, name, lhs, rhs) ->
    Format.sprintf
      "%s %s = (%s) in (%s)"
      (show_pretty_rec_flag rf)
      (show_name name)
      (show_pretty_ast lhs)
      (show_pretty_ast rhs)
  | Abstraction (var, expr) ->
    Format.sprintf "fun %s -> %s" (show_name var) (show_pretty_ast expr)
  | Application (lhs, rhs) ->
    Format.sprintf "((%s) (%s))" (show_pretty_ast lhs) (show_pretty_ast rhs)
;;
