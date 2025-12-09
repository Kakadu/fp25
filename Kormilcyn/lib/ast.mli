[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string

type binop =
  | Add
  | Mul
  | Sub
  | Div
  | Leq
  | Eq

(* TODO: meaningful exceptions *)

(** The main type for our AST (дерева абстрактного синтаксиса) *)
type 'name t =
  | Var of 'name (** Variable [x] *)
  | Fun of 'name * 'name t
  | App of 'name t * 'name t
  | Int of int
  | Neg of 'name t
  | Bin of binop * 'name t * 'name t
  | Let of 'name * 'name t * 'name t
  | If of 'name t * 'name t * 'name t
  | LetRec of 'name * 'name t * 'name t
  (** In type definition above the 3rd constructor is intentionally without documentation
    to test linter *)
(* Application [f g ] *)
