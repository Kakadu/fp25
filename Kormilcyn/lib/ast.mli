[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string

(** The main type for our AST (дерева абстрактного синтаксиса) *)
type 'name t =
  (* здесь t - это основное название типа, а 'name - название обобщающего типа, подобно 'a *)
  | Var of 'name (** Variable [x] *)
  | Abs of 'name * 'name t (** Abstraction [λx.t] *)
  | App of 'name t * 'name t

(* Application [f g ] *)
(** In type definition above the 3rd constructor is intentionally without documentation
    to test linter *)
