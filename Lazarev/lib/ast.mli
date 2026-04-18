[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Type for name of variable, used in variable, abstraction, let, letrec *)
type name =
  | Wildcard (** Wildcard name [_] *)
  | Real of string (** Real name (any string) *)

(** Type for any unary operation in language *)
type unary_operation =
  | Neg (** Integer negation [-x] *)
  | Not (** Logical inversion [!x] *)

(** Type for any binary operation in language *)
type binary_operation =
  | Add (** Addition [a + b] *)
  | Sub (** Subtraction [a - b] *)
  | Mul (** Multiplication [a * b] *)
  | Div (** Division [a / b] *)
  | Mod (** Remainder [a mod b] *)
  | And (** [a && b] *)
  | Or (** [a || b] *)
  | Equal (** Comparing on equal [a = b] *)
  | NotEqual (** Comparing on not eqaul [a <> b] *)
  | Less (** Compare on less [a < b] *)
  | LessEqual (** Compare on less or equal [a <= b] *)
  | Greater (** Compare on greater [a > b] *)
  | GreaterEqual (** Compare on greater or equal [a >= b] *)

(** Type for indicating recursive let expression *)
type let_mnemonic =
  | Let (** Common [let] *)
  | LetRec (** Recursive [let rec] *)

(** Type for abstract-syntax tree node *)
type t =
  | Unit (** Unit literal *)
  | Int of int (** Integer literal [42] *)
  | Bool of bool (** Boolean literal [true] *)
  | Var of name (** Variable [x] *)
  | Tuple of t * t * t list (** Tuple [(x, y, ..., z)] *)
  | UnaryOp of unary_operation * t (** Unary operation [..y] *)
  | BinaryOp of binary_operation * t * t (** Binary operation [x .. y] *)
  | IfThenElse of t * t * t (** If-then-else expression [if ... then ... else ...] *)
  | LetExpr of let_mnemonic * name * t * t (** Let expression [let x = ... in ...] *)
  | Abstraction of name * t (** Lambda abstraction [fun x -> ...] *)
  | Application of t * t (** Application [(f x)] *)

(** Shows AST in verbose format *)
val show_ast_verbose : t -> string

(** Shows AST in human readable format *)
val show_ast : t -> string

(** Shows name of any named entity in AST *)
val show_name : name -> string
