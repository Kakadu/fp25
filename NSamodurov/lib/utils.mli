[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  [ `ParsingError of string
  | `OccursCheck of Type.ty * Type.ty
  | `UnifyError of Type.ty * Type.ty
  | `UnboundVariable of int
  | `AbstractionExpected of Type.ty
  | `UsingReservedVariable of int
  | `ReservedError
  | `InterpretError of string
  ]
[@@deriving show { with_path = false }]

val free_vars : equal:('a -> 'a -> bool) -> 'a Ast.t -> 'a list
val is_free_in : string -> string Ast.t -> bool
(* val is_free_in_brujin : Ast.brujin -> Ast.brujin Ast.t -> bool *)

(** Smart constructors *)
