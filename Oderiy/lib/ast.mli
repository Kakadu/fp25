[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string

(** Binary operators *)
type binop =
  | Add (** Addition: [+] *)
  | Mul (** Multiplication: [*] *)
  | Sub (** Subtraction: [-] *)
  | Div (** Division: [/] *)
  | Leq (** Less than or equal: [<=] *)
  | Lt (** Less than: [<] *)
  | Eq (** Equal: [=] *)
  | Geq (** Greater than or equal: [>=] *)
  | Gt (** Greater than: [>] *)
[@@deriving show { with_path = false }]

(** Abstract syntax tree for miniML expressions *)
type 'name t =
  | Var of 'name (** Variable reference: represents a variable by its name *)
  | Fun of 'name * 'name t
  (** Function abstraction: [fun x -> body] creates a function with parameter [x] and body *)
  | App of 'name t * 'name t
  (** Function application: [f arg] applies function [f] to argument [arg] *)
  | Int of int (** Integer literal: constant integer value *)
  | Neg of 'name t (** Unary negation: [-e] negates the value of expression [e] *)
  | Bin of binop * 'name t * 'name t
  (** Binary operation: [e1 op e2] applies binary operator [op] to [e1] and [e2] *)
  | Let of 'name * 'name t * 'name t
  (** Let binding: [let x = e1 in e2] binds [x] to [e1] in scope of [e2] *)
  | If of 'name t * 'name t * 'name t
  (** Conditional: [if cond then e1 else e2] evaluates to [e1] if [cond] is non-zero, otherwise [e2] *)
  | LetRec of 'name * 'name t * 'name t
  (** Recursive let: [let rec f = e1 in e2] creates recursive binding of [f] to [e1] in [e2] *)
  | Fix (** Fixed-point operator: [fix] enables recursion without explicit [let rec] *)
