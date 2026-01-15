[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string

type binop =
  | Add (** Integer addition *)
  | Mul (** Integer multiplication *)
  | Sub (** Integer subtraction *)
  | Div (** Integer division *)
  | Leq (** Integer less-or-equal comparison *)
  | Eq (** Integer equality comparison *)
  | Geq (** Integer greater-or-equal comparison *)

(** The main type for our AST (дерева абстрактного синтаксиса) *)
type 'name t =
  | Var of 'name (** Variable [x] *)
  | Fun of 'name * 'name t (** Function [fun x -> ...] *)
  | App of 'name t * 'name t (** Application [f g] *)
  | Int of int (** Integer literal *)
  | Neg of 'name t (** Unary minus *)
  | Bin of binop * 'name t * 'name t (** Binary operator application *)
  | Let of 'name * 'name t * 'name t (** Let binding *)
  | If of 'name t * 'name t * 'name t (** Conditional expression *)
  | LetRec of 'name * 'name t * 'name t (** Recursive let binding *)
  | Fix (** Fixed-point operator *)
