[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val pp_error
  :  Format.formatter
  -> [> `AbstractionExpected of Type.ty
     | `InterpretError of string
     | `OccursCheck of Type.ty * Type.ty
     | `ParsingError of string
     | `ReservedError
     | `UnboundVariable of int
     | `UnifyError of Type.ty * Type.ty
     | `UsingReservedVariable of int
     ]
  -> unit

val free_vars : equal:('a -> 'a -> bool) -> 'a Ast.t -> 'a list
val is_free_in : string -> string Ast.t -> bool
(* val is_free_in_brujin : Ast.brujin -> Ast.brujin Ast.t -> bool *)
