(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = string [@@deriving show { with_path = false }]

type constant =
  | Const_int of int
  (* | Const_bool of bool *)
  | Const_unit
[@@deriving show { with_path = false }]

(** recursive flag *)
type rec_flag =
  | Recursive
  | NonRecursive
[@@deriving show { with_path = false }]

type binary_op =
  | Add
  | Mult
  | Sub
  | Div
  | Gt
  | Lt
  | Eq
  | Neq
  | Gte
  | Lte
[@@deriving show { with_path = false }]

type unary_op =
  | Negative
  | Positive
[@@deriving show { with_path = false }]

type type_annot =
  | Type_int
  | Type_unit
  | Type_var of string
  | Type_arrow of type_annot * type_annot
  | Type_option of type_annot
[@@deriving show { with_path = false }]

type pattern =
  | Pat_any
  | Pat_var of ident
  | Pat_constant of constant
  | Pat_option of pattern option
  | Pat_constraint of type_annot * pattern
[@@deriving show { with_path = false }]

type expression =
  | Expr_const of constant
  | Expr_ident of ident
  | Expr_option of expression option
  | Expr_constraint of type_annot * expression
  | Expr_binop of binary_op * expression * expression
  | Expr_unop of unary_op * expression
  | Expr_fun of pattern * expression
  | Expr_apply of expression * expression
  | Expr_if of expression * expression * expression option
  | Expr_let of rec_flag * value_binding * value_binding list * expression
  | Expr_function of case * case list
  | Expr_match of expression * case * case list
[@@deriving show { with_path = false }]

and value_binding =
  { vb_pat : pattern
  ; vb_expr : expression
  }
[@@deriving show { with_path = false }]

and case =
  { case_pat : pattern
  ; case_expr : expression
  }
[@@deriving show { with_path = false }]

type structure_item =
  | Str_eval of expression
  | Str_value of rec_flag * value_binding * value_binding list
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]
