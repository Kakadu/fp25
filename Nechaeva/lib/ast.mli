(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string

(** Recursion flag for let bindings *)
type rec_flag =
  | NonRecursive (** [let x = e1 in e2] *)
  | Recursive (** [let rec f = e1 in e2] *)

(** Binary operations *)
type binop =
  | Plus (** Integer addition: + *)
  | Minus (** Integer subtraction: - *)
  | Mul (** Integer multiplication: * *)
  | Div (** Integer division: / *)

(** Comparison operations *)
type compop =
  | Equal (** Equal: = *)
  | NotEqual (** Not equal: <> *)
  | Less (** Less than: < *)
  | LessEq (** Less or equal: <= *)
  | Greater (** Greater than: > *)
  | GreaterEq (** Greater or equal: >= *)

(** Constant values *)
type constant =
  | Int of int (** Integer literal *)
  | Unit (** Unit value *)

(** Unary operations *)
type unop = Neg (** Unary minus: -e *)

(** Expressions *)
type expr =
  | Const of constant (** Constant value *)
  | Var of name (** Variable reference *)
  | Abs of name list * expr (** Multi-argument abstraction: [fun x y -> e] *)
  | App of expr * expr (** Function application: [f arg] *)
  | Let of rec_flag * name * expr * expr (** Let binding: [let[rec] x = e1 in e2] *)
  | BinOp of binop * expr * expr (** Binary arithmetic operation *)
  | UnOp of unop * expr (** Unary operation *)
  | Comp of compop * expr * expr (** Comparison operation *)
  | If of expr * expr * expr (** Conditional: [if cond then e1 else e2] *)
