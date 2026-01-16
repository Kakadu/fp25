(** Copyright 2021-2025, Kakadu and contributors **)

(** SPDX-License-Identifier: LGPL-3.0-or-later **)

[@@@ocaml.text "/*"]

(** Core AST for the miniML language.

    Conventions:
    - Call-by-value evaluation.
    - No native booleans: `0` is false, any non-zero int is true.
    - `fun a b -> body` is treated as `fun a -> fun b -> body`.
    - Top-level `let` has no body; local `let ... in ...` carries a body. **)

(** Binding scope: whether a `let` lives at the top level (no body) or inside an
    expression (has body). **)
type var_scope =
  | LocalVar (** Bound inside an expression (has a body). **)
  | GlobalVar (** Top-level binding without a body. **)
[@@deriving show { with_path = false }]

(** Primitive constants. Only ints carry payload; `Unit` is a placeholder for
    statements and unused branches. **)
type const =
  | Int of int (** Integer literal. **)
  | Unit (** Placeholder for statements/empty value. **)
[@@deriving show { with_path = false }]

(** Variable name. Patterns are not supported beyond bare identifiers. **)
type name = string [@@deriving show { with_path = false }]

(** Binary operations on integers. All results are ints (1/0 for comparisons). **)
type operation_id =
  | OpAdd (** + **)
  | OpSub (** - **)
  | OpMul (** * **)
  | OpDiv (** / **)
  | OpEq (** = **)
  | OpGt (** > **)
  | OpLt (** < **)
  | OpGte (** >= **)
  | OpLte (** <= **)
[@@deriving show { with_path = false }]

(** Distinguish let from let rec. **)
type binding_kind =
  | NonRec (** let x = e **)
  | Rec (** let rec f = fun x -> e **)
[@@deriving show { with_path = false }]

(** Expressions of the language. **)
type expression =
  | Const of const (** Literal constants. **)
  | Var of name (** Variable lookup. **)
  | Let of var_scope * binding_kind * name * expression * expression option
  (** let/let rec:
      - var_scope marks global vs local binding (informational);
      - binding_kind distinguishes `let` vs `let rec`;
      - name is the bound variable;
      - expression is the bound value (possibly a sugared multi-arg fun);
      - expression option is the body (`None` for top-level declarations). **)
  | Fun of name * expression (** Lambda abstraction: fun x -> e. **)
  | App of expression * expression (** Application: f a (left-associative). **)
  | BinOp of operation_id * expression * expression (** Integer operation. **)
  | If of expression * expression * expression option
  (** Conditional: `0` is false, non-zero is true; else is optional. **)
[@@deriving show { with_path = false }]

(** Sugars and notes:
    - CBV evaluation strategy.
    - Bool-on-int encoding: 0 -> false, non-zero -> true.
    - Multi-argument lambdas are desugared into nested single-arg `Fun`. **)
