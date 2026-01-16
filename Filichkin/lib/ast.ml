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
  | NotEqual
  | More
  | Less
  | EMore
  | ELess
  | And
  | Or
[@@deriving show { with_path = false }]

type rec_flag =
  | NonRec
  | Rec
[@@deriving show { with_path = false }]

type unop =
  | Neg
  | Not
[@@deriving show { with_path = false }]

type ident = string [@@deriving show { with_path = false }]

type type_expr =
  | TEInt
  | TEBool
  | TEUnit
  | TEVar of ident
  | TEArrow of type_expr * type_expr
  | TETuple of type_expr list
  | TEConstr of ident * type_expr list
[@@deriving show { with_path = false }]

type constr_decl =
  { ctor_name : ident
  ; ctor_args : type_expr list
  }
[@@deriving show { with_path = false }]

type type_decl =
  { type_name : ident
  ; type_params : ident list
  ; constructors : constr_decl list
  }
[@@deriving show { with_path = false }]

type pattern =
  | PVar of ident
  | PTuple of pattern list
  | PConstr of ident * pattern list
  | PWildcard
[@@deriving show { with_path = false }]

type expr =
  | Int of int
  | Var of ident
  | Constr of ident
  | BinOp of binop * expr * expr
  | Bool of bool
  | UnOp of unop * expr
  | If of expr * expr * expr
  | Let of rec_flag * pattern * expr * expr option
  | Abs of pattern * expr
  | App of expr * expr
  | Tuple of expr list
  | Match of expr * (pattern * expr) list
[@@deriving show { with_path = false }]

type toplevel =
  | TLExpr of expr
  | TLType of type_decl
[@@deriving show { with_path = false }]
