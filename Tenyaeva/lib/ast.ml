(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck
open Base
open Gen
open Stdlib

type ident = string [@@deriving show { with_path = false }]

let gen_char = map Char.chr (int_range (Char.code 'a') (Char.code 'z'))
let gen_ident = string_size (int_range 1 8) ~gen:gen_char

type constant =
  | Const_int of (int[@gen small_int])
  | Const_unit
[@@deriving show { with_path = false }, qcheck]

(** recursive flag *)
type rec_flag =
  | Recursive
  | NonRecursive
[@@deriving show { with_path = false }, qcheck]

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
[@@deriving show { with_path = false }, qcheck]

type unary_op =
  | Negative
  | Positive
[@@deriving show { with_path = false }, qcheck]

type type_annot =
  | Type_int
  | Type_unit
  | Type_var of (ident[@gen gen_ident])
  | Type_arrow of type_annot * type_annot
  | Type_option of type_annot
[@@deriving show { with_path = false }, qcheck]

type pattern =
  | Pat_any
  | Pat_var of (ident[@gen gen_ident])
  | Pat_constant of constant
  | Pat_option of pattern option
  | Pat_constraint of type_annot * pattern
[@@deriving show { with_path = false }, qcheck]

type expression =
  | Expr_const of constant
  | Expr_ident of (ident[@gen gen_ident])
  | Expr_option of
      (expression option[@gen QCheck.Gen.option (gen_expression_sized (n / 5))])
  | Expr_constraint of type_annot * (expression[@gen gen_expression_sized (n / 5)])
  | Expr_binop of binary_op * expression * expression
  | Expr_unop of unary_op * expression
  | Expr_fun of (pattern[@gen gen_pattern_sized (n / 3)]) * expression
  | Expr_apply of
      (expression[@gen gen_expression_sized (n / 5)])
      * (expression[@gen gen_expression_sized (n / 5)])
  | Expr_if of
      (expression[@gen gen_expression_sized (n / 5)])
      * (expression[@gen gen_expression_sized (n / 5)])
      * (expression option[@gen QCheck.Gen.option (gen_expression_sized (n / 5))])
  | Expr_let of
      rec_flag
      * value_binding
      * (value_binding list
        [@gen QCheck.Gen.(list_size (0 -- 1) (gen_value_binding_sized (n / 20)))])
      * expression
  | Expr_function of
      case * (case list[@gen QCheck.Gen.(list_size (0 -- 1) (gen_case_sized (n / 20)))])
  | Expr_match of
      (expression[@gen gen_expression_sized (n / 5)])
      * (case[@gen gen_case_sized (n / 5)])
      * (case list[@gen QCheck.Gen.(list_size (0 -- 1) (gen_case_sized (n / 20)))])
[@@deriving show { with_path = false }, qcheck]

and value_binding =
  { vb_pat : (pattern[@gen gen_pattern_sized (n / 5)])
  ; vb_expr : (expression[@gen gen_expression_sized (n / 5)])
  }
[@@deriving show { with_path = false }, qcheck]

and case =
  { case_pat : (pattern[@gen gen_pattern_sized (n / 5)])
  ; case_expr : (expression[@gen gen_expression_sized (n / 5)])
  }
[@@deriving show { with_path = false }, qcheck]

let gen_expression =
  let* n = small_nat in
  gen_expression_sized n
;;

type structure_item =
  | Str_eval of expression
  | Str_value of
      rec_flag
      * value_binding
      * (value_binding list[@gen list_size (1 -- 2) gen_value_binding])
[@@deriving show { with_path = false }, qcheck]

type structure = (structure_item list[@gen list_size (1 -- 2) gen_structure_item])
[@@deriving show { with_path = false }, qcheck]
