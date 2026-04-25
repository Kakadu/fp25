[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitry Arzhaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Constants supported by the language. *)
type const =
  | IConst of int (** Integer constant *)
  | FConst of float (** Floating-point constant *)
  | BConst of bool (** Boolean constant *)

(** Binary operators. *)
type binop =
  | Add (** Integer addition: + *)
  | Sub (** Integer subtraction: - *)
  | Mul (** Integer multiplication: * *)
  | Div (** Integer division: / *)
  | Eq (** Equality: = *)
  | Neq (** Inequality: <> *)
  | Leq (** Less or equal: <= *)
  | Geq (** Greater or equal: >= *)
  | Lt (** Less than: < *)
  | Gt (** Greater than: > *)
  | And (** Boolean and: && *)
  | Or (** Boolean or: || *)
  | AddF (** Float addition: +. *)
  | SubF (** Float subtraction: -. *)
  | MulF (** Float multiplication: *. *)
  | DivF (** Float division: /. *)

(** Indicates whether a binding is recursive. *)
type reclabel =
  | Recursive (** Recursive binding (introduced with [let rec]) *)
  | Nonrecursive (** Non-recursive binding *)

(** Expressions of the language. *)
type expr =
  | EConst of const (** Constant *)
  | EVar of string (** Variable *)
  | EBinOp of binop * expr * expr (** Binary operation *)
  | ELet of reclabel * letbind * expr (** Let-binding: [let x = e1 in e2] *)
  | EIf of expr * expr * expr (** Conditional expression *)
  | EFun of expr * expr (** Function abstraction *)
  | EApp of expr * expr (** Function application *)

(** Let-binding: left-hand side and right-hand side expression. *)
and letbind = Bind of expr * expr (** Binding pair: lhs = rhs *)

(** Top-level constructs. *)
type toplevel =
  | TopLet of reclabel * letbind (** Top-level let binding *)
  | TopExpr of expr (** Top-level expression *)

(** Pretty-printer for constants. *)
val pp_const : Format.formatter -> const -> unit

(** Pretty-printer for binary operators. *)
val pp_binop : Format.formatter -> binop -> unit

(** Pretty-printer for recursion labels. *)
val pp_reclabel : Format.formatter -> reclabel -> unit

(** Pretty-printer for expressions. *)
val pp_expr : Format.formatter -> expr -> unit

(** Pretty-printer for let-bindings. *)
val pp_letbind : Format.formatter -> letbind -> unit

(** Pretty-printer for toplevel constructs. *)
val pp_toplevel : Format.formatter -> toplevel -> unit
