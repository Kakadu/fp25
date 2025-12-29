[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type binop =
  | Plus (** Addition: `+` *)
  | Minus (** Subtraction: `-` *)
  | Mult (** Multiplication: `*` *)
  | Div (** Division: `/` *)
  | Equal (** Equality: `=` *)
  | NotEqual (** Not Equal `<>` *)
  | More (** Greater than: `>` *)
  | Less (** Less than: `<` *)
  | EMore (** Greater than or equal: `>=` *)
  | ELess (** Less than or equal: `<=` *)
  | And (** `&&` *)
  | Or (** `||` *)

type rec_flag =
  | NonRec (** Non-recursive binding *)
  | Rec (** Recursive binding *)

type unop =
  | Neg (** Unary negation *)
  | Not (** Logical NOT *)

type ident = string [@@deriving show { with_path = false }]

type expr =
  | Int of int (** Integer literal *)
  | Var of ident (** Variable name *)
  | BinOp of binop * expr * expr
  | Bool of bool (** Boolean literal *)
  (** Binary operation: operator, left and right operands *)
  | UnOp of unop * expr (** Unary operation: operator, operand *)
  | If of expr * expr * expr option
  (** Conditional expression: condition, then branch, optional else branch *)
  | Let of rec_flag * string * expr * expr option
  (** Let expression: recursiveness flag, name, bound expression, body (optional)
      The last parameter is the optional expression body (for parsing convenience) *)
  | Abs of ident * expr
  (** Abstraction (lambda function): parameter list, function body
      Syntactic sugar for functions with multiple arguments *)
  | App of expr * expr (** Application (function call): function, argument *)
