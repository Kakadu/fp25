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
[@@deriving show { with_path = false }]

type rec_flag =
  | NonRec (** Non-recursive binding *)
  | Rec (** Recursive binding *)
[@@deriving show { with_path = false }]

type unop =
  | Neg (** Unary negation *)
  | Not (** Logical NOT *)
[@@deriving show { with_path = false }]

type ident = string [@@deriving show { with_path = false }]

type pattern =
  | PVar of ident
  | PTuple of pattern list
[@@deriving show { with_path = false }]

type expr =
  | Int of int (** Integer literal *)
  | Var of ident (** Variable name *)
  | BinOp of binop * expr * expr
  (** Binary operation: operator, left and right operands *)
  | Bool of bool (** Boolean literal *)
  | UnOp of unop * expr (** Unary operation: operator, operand *)
  | If of expr * expr * expr
  (** Conditional expression: condition, then branch, optional else branch *)
  | Let of rec_flag * pattern * expr * expr option
  (** Let expression: recursiveness flag, name, bound expression, body (optional)
      The last parameter is the optional expression body (for parsing convenience) *)
  | Abs of pattern * expr
  (** Abstraction (lambda function): parameter,
      Syntactic sugar for functions with multiple arguments *)
  | App of expr * expr (** Application (function call): function, argument *)
  | Tuple of expr list
[@@deriving show { with_path = false }]
