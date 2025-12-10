[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string

(** Binary operators *)
type binop =
  | Add (** [+] *)
  | Sub (** [-] *)
  | Mul (** [*] *)
  | Div (** [/] *)
  | Mod (** [%] *)
  | Eq (** [=] *)
  | Neq (** [<>] *)
  | Lt (** [<] *)
  | Gt (** [>] *)
  | Leq (** [<=] *)
  | Geq (** [>=] *)

(** The main type for our AST (дерева абстрактного синтаксиса) *)
type 'name t =
  | Var of 'name (** Variable [x] *)
  | Abs of 'name * 'name t (** Abstraction [λx.t] *)
  | App of 'name t * 'name t (** Application [f g ] *)
  | Int of int (** Integer literal [42] *)
  | BinOp of binop * 'name t * 'name t (** Binary operation [e1 + e2] *)
