[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Equal
  | More
  | Less
  | EMore
  | ELess

type rec_flag =
  | NonRec
  | Rec

type patrn =
  | Var of string
  | Other of string

(** Fixed-point operator (for recursion): Fix f computes the fixed point of f *)

type expr =
  | Int of int (** Integer literal *)
  | Var of string (** Variable name *)
  | BinOp of binop * expr * expr
  (** Binary operation: operator, left and right operands *)
  | If of expr * expr * expr option
  (** Conditional expression: condition, then branch, optional else branch *)
  | Let of rec_flag * string * expr * expr option
  (** Let expression: recursiveness flag, name, bound expression, body (optional)
      The last parameter is the optional expression body (for parsing convenience) *)
  | Abs of expr * expr
  (** Abstraction (lambda function): parameter list, function body
      Syntactic sugar for functions with multiple arguments *)
  | App of expr * expr (** Application (function call): function, argument *)
