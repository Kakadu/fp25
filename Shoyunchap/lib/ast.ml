[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Binding scope: top-level vs local. *)
type var_scope =
  | LocalVar
  | GlobalVar
[@@deriving show { with_path = false }]

(** Primitive constants. Only ints are real values; unit is used for statements. *)
type const =
  | Int of int
  | Unit
[@@deriving show { with_path = false }]

(** Variable name. No other patterns to keep syntax lean. *)
type name = string [@@deriving show { with_path = false }]

(** Binary operations on integers. *)
type operation_id =
  | OpAdd  (** + *)
  | OpSub  (** - *)
  | OpMul  (** * *)
  | OpDiv  (** /, integer division *)
  | OpEq  (** = *)
  | OpGt  (** > *)
  | OpLt  (** < *)
  | OpGte  (** >= *)
  | OpLte  (** <= *)
[@@deriving show { with_path = false }]

(** Distinguish let from let rec. *)
type binding_kind =
  | NonRec  (** let x = e *)
  | Rec  (** let rec f = fun x -> e *)
[@@deriving show { with_path = false }]

(** Expressions of the language. *)
type expression =
  | Const of const  (** Int and unit. *)
  | Var of name
  | Let of var_scope * binding_kind * name * expression * expression option
      (** let/let rec:
          - var_scope marks global vs local binding;
          - binding_kind picks recursion;
          - name is the bound variable;
          - expression is the bound value;
          - expression option is the body (None for top-level statements). *)
  | Fun of name * expression  (** Abstraction: fun x -> e. *)
  | App of expression * expression  (** Application: f a. *)
  | BinOp of operation_id * expression * expression  (** Integer operation. *)
  | If of expression * expression * expression option
      (** Condition: 0 is false, non-zero is true; else is optional. *)
[@@deriving show { with_path = false }]

(** 
    - CBV 
    - Bool on int: 0 - False, != 0 - True
    fun a b -> ... - sugar for fun a -> fun b -> ...
    *)