(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck
open Base
open Gen
open Stdlib

type ident = string [@@deriving show { with_path = false }] (* identifier *)

let gen_char = map Char.chr (int_range (Char.code 'a') (Char.code 'z'))
let gen_ident = string_size (int_range 1 8) ~gen:gen_char

type constant =
  | Const_int of (int[@gen small_int]) (** integer, e.g. 122 *)
  | Const_bool of bool (** boolean, e.g. true *)
  | Const_unit (** [()] *)
[@@deriving show { with_path = false }, qcheck]

type rec_flag =
  | Recursive (** recursive *)
  | NonRecursive (** non-recursive *)
[@@deriving show { with_path = false }, qcheck]

type binary_op =
  | Add (** [+] *)
  | Mult (** [*] *)
  | Sub (** [-] *)
  | Div (** [/] *)
  | Gt (** [>] *)
  | Lt (** [<] *)
  | Eq (** [=] *)
  | Neq (** [<>] *)
  | Gte (** [>=] *)
  | Lte (** [<=] *)
[@@deriving show { with_path = false }, qcheck]

type unary_op =
  | Negative (** unary minus, e.g. -5 *)
  | Positive (** unary plus, e.g. +5 *)
  | Not (** [not] *)
[@@deriving show { with_path = false }, qcheck]

type type_annot =
  | Type_int (** integer type - [int] *)
  | Type_bool (** boolean type - [bool] *)
  | Type_unit (** unit type - [unit] *)
  | Type_var of (ident[@gen gen_ident]) (** variable type *)
  | Type_arrow of type_annot * type_annot (** arrow type *)
  | Type_option of (type_annot[@gen gen_type_annot_sized (n / 20)]) (** type option *)
[@@deriving show { with_path = false }, qcheck]

type pattern =
  | Pat_any (** matches any value without binding it - [_] *)
  | Pat_var of (ident[@gen gen_ident])
  (** matches any value and binds it to a variable, e.g. x *)
  | Pat_constant of constant (** matches a constant value, e.g. 42, true *)
  | Pat_option of pattern option (** matches an optional pattern, e.g. Some x or None *)
  | Pat_constraint of type_annot * pattern (** typed pattern, e.g. a: int *)
[@@deriving show { with_path = false }, qcheck]

type expression =
  | Expr_const of constant (** constant, e.g. 10*)
  | Expr_ident of (ident[@gen gen_ident]) (** variable, e.g. x *)
  | Expr_option of
      (expression option[@gen QCheck.Gen.option (gen_expression_sized (n / 5))])
  (** optonal expression, e.g. Some x*)
  | Expr_constraint of type_annot * (expression[@gen gen_expression_sized (n / 5)])
  (** typed expression, e.g. a: int *)
  | Expr_binop of binary_op * expression * expression (** binary operation, e.g. 1 + 5*)
  | Expr_unop of unary_op * expression (** unary operation, e.g. -7 *)
  | Expr_fun of (pattern[@gen gen_pattern_sized (n / 3)]) * expression
  (** function, e.g. fun (x, y) -> x + y *)
  | Expr_apply of
      (expression[@gen gen_expression_sized (n / 5)])
      * (expression[@gen gen_expression_sized (n / 5)])
  (** application, e.g. (fun (x, y) -> x + y) (1, 2) *)
  | Expr_if of
      (expression[@gen gen_expression_sized (n / 5)])
      * (expression[@gen gen_expression_sized (n / 5)])
      * (expression option[@gen QCheck.Gen.option (gen_expression_sized (n / 5))])
  (** conditional expression, e.g. if a then b else c*)
  | Expr_let of
      rec_flag
      * value_binding
      * (value_binding list
        [@gen QCheck.Gen.(list_size (0 -- 1) (gen_value_binding_sized (n / 20)))])
      * expression (** let, e.g. let x = 5 *)
  | Expr_function of
      case * (case list[@gen QCheck.Gen.(list_size (0 -- 1) (gen_case_sized (n / 20)))])
  (** function, e.g. fun (x, y) -> x + y *)
  | Expr_match of
      (expression[@gen gen_expression_sized (n / 5)])
      * (case[@gen gen_case_sized (n / 5)])
      * (case list[@gen QCheck.Gen.(list_size (0 -- 1) (gen_case_sized (n / 20)))])
  (** pattern matching, e.g. match x with | 0 -> "zero" | _ -> "nonzero" *)
[@@deriving show { with_path = false }, qcheck]

and value_binding =
  { vb_pat : (pattern[@gen gen_pattern_sized (n / 5)])
  (** the pattern being bound, e.g. x, (a, b) *)
  ; vb_expr : (expression[@gen gen_expression_sized (n / 5)])
  (** the expression being assigned, e.g. 42, fun x -> x + 1 *)
  }
[@@deriving show { with_path = false }, qcheck]

and case =
  { case_pat : (pattern[@gen gen_pattern_sized (n / 5)])
  (** the pattern to match, e.g. x, _ *)
  ; case_expr : (expression[@gen gen_expression_sized (n / 5)])
  (** the expression to evaluate if the pattern matches *)
  }
[@@deriving show { with_path = false }, qcheck]

let gen_expression =
  let* n = small_nat in
  gen_expression_sized n
;;

type structure_item =
  | Str_eval of expression (** an expression to be evaluated but not bound, e.g. 1 + 2*)
  | Str_value of
      rec_flag
      * value_binding
      * (value_binding list[@gen list_size (1 -- 2) gen_value_binding])
  (** a value or function binding, e.g. let x = 1*)
[@@deriving show { with_path = false }, qcheck]

(** full program *)
type structure = (structure_item list[@gen list_size (1 -- 2) gen_structure_item])
[@@deriving show { with_path = false }, qcheck]
