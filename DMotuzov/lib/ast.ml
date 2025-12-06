[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Name of function or variable *)
type identificator = string [@@deriving show { with_path = false }]

type constant =
  | Const_int of int (** Integer literal, e.g. `42`, `-7`, `0` *)
  | Const_unit (** Unit value `()` representing absence of meaningful value *)
[@@deriving show { with_path = false }]

(** Binary operators *)
type binary_op =
  | Plus (** '+' *)
  | Sub (** '-' *)
  | Mul (** '*' *)
  | Div (** '/' *)
[@@deriving show { with_path = false }]

(** Expressions in the AST *)
type expression =
  | Expr_var of identificator (** Variable, e.g. `x` *)
  | Expr_const of constant (** Constant, e.g. integer or unit *)
  | Expr_binary_op of binary_op * expression * expression
  (** Binary operation, e.g. `e1 + e2` *)
  | Expr_conditional of expression * expression * expression
  (** Conditional expression, e.g. `if e1 then e2 else e3` *)
  | Expr_let_in of identificator * expression * expression
  (** Local binding, e.g. `let x = e1 in e2` *)
  | Expr_let_rec_in of identificator * expression * expression
  (** Local rec binding, e.g. `let rec x = e1 in e2` *)
  | Expr_fun of identificator * expression
  (** Abstraction (function), e.g. `fun x -> e` *)
  | Expr_ap of expression * expression list (** Function application, e.g. `f x` *)
  | Expr_fix of expression
  (** Fixed-point combinator for recursion, e.g. `fix (fun f x -> ...)`
      Enables defining recursive functions without explicit `let rec`. *)
[@@deriving show { with_path = false }]

(** Top-level definitions in a program *)
type toplevel =
  | Top_let of identificator * expression
  (** Global non-recursive binding, e.g. `let x = 42;;` *)
  | Top_let_rec of identificator * expression
  (** Global recursive binding, e.g. `let rec fact n = ...;;` *)
[@@deriving show { with_path = false }]

(** Complete program structure - sequence of top-level definitions *)
type structure = toplevel list [@@deriving show { with_path = false }]
