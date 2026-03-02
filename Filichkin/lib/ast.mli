[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type binop =
  | Plus (** Addition: `+` *)
  | Minus (** Subtraction: `-` *)
  | Mult (** Multiplication: `*` *)
  | Div (** Division: `/` *)
  | Equal (** Equality: `=` *)
  | NotEqual (** Not Equal `<>` *)
  | More (** Greater than: `>` *)
  | Less (** Less than: `<` *)
  | EMore (** Greater than or equal: `>=` *)
  | ELess (** Less than or equal: `<=` *)
  | And (** `&&` *)
  | Or (** `||` *)
[@@deriving show { with_path = false }]

type rec_flag =
  | NonRec (** Non-recursive binding *)
  | Rec (** Recursive binding *)
[@@deriving show { with_path = false }]

type unop =
  | Neg (** Unary negation *)
  | Not (** Logical NOT *)
[@@deriving show { with_path = false }]

type ident = string [@@deriving show { with_path = false }]

type type_expr =
  | TEInt (** Integer type *)
  | TEBool (** Boolean type *)
  | TEUnit (** Unit type *)
  | TEVar of ident (** Type variable *)
  | TEArrow of type_expr * type_expr (** Function type *)
  | TETuple of type_expr list (** Tuple type *)
  | TEConstr of ident * type_expr list (** Type constructor application *)
[@@deriving show { with_path = false }]

type constr_decl =
  { ctor_name : ident
  ; ctor_args : type_expr list
  }

type type_decl =
  { type_name : ident
  ; type_params : ident list
  ; constructors : constr_decl list
  }

type pattern =
  | PVar of ident (** Variable pattern *)
  | PTuple of pattern list (** Tuple pattern *)
  | PConstr of ident * pattern list (** Constructor pattern *)
  | PWildcard (** Wildcard pattern *)
[@@deriving show { with_path = false }]

type expr =
  | Int of int (** Integer literal *)
  | Var of ident (** Variable name *)
  | Constr of ident (** Constr name *)
  | BinOp of binop * expr * expr
  (** Binary operation: operator, left and right operands *)
  | Bool of bool (** Boolean literal *)
  | UnOp of unop * expr (** Unary operation: operator, operand *)
  | If of expr * expr * expr
  (** Conditional expression: condition, then branch, optional else branch *)
  | Let of rec_flag * pattern * expr * expr option
  (** Let expression: recursiveness flag, name, bound expression, body (optional)
      The last parameter is the optional expression body (for parsing convenience) *)
  | Abs of pattern * expr
  (** Abstraction (lambda function): parameter,
      Syntactic sugar for functions with multiple arguments *)
  | App of expr * expr (** Application (function call): function, argument *)
  | Tuple of expr list (** Tuple expression *)
  | Match of expr * (pattern * expr) list (** Pattern matching *)
[@@deriving show { with_path = false }]

type toplevel =
  | TLExpr of expr (** Top-level expression *)
  | TLType of type_decl (** Top-level type declaration *)
[@@deriving show { with_path = false }]
