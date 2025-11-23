[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Ast
open Type

type error =
  [ `ParsingError of string
  | `OccursCheck of ty * ty
  | `UnifyError of ty * ty
  | `UnboundVariable of int
  | `AbstractionExpected of ty
  | `UsingReservedVariable of int
  | `ReservedError
  | `InterpretError of string
  ]
[@@deriving show { with_path = false }]

let pp_error ppf = function
  | `ParsingError s -> Format.fprintf ppf "%s" s
  | `OccursCheck (a, b) -> Format.fprintf ppf "Occurs error: %a %a" pp_ty a pp_ty b
  | `UnifyError (a, b) ->
    Format.fprintf ppf "Unification error: (%a) (%a)" pp_ty a pp_ty b
  | `UnboundVariable i -> Format.fprintf ppf "Unbound variable: %d" i
  | `AbstractionExpected t -> Format.fprintf ppf "AbstractionExpected: %a" pp_ty t
  | `UsingReservedVariable i -> Format.fprintf ppf "UsingReservedVariable: %d" i
  | `ReservedError -> Format.fprintf ppf "Reserved variable limit exceeded"
  | `InterpretError s -> Format.fprintf ppf "Can't interpret: %s\n" s
;;

let list_remove x ~equal = List.filter ~f:(fun a -> not (equal a x))

let free_vars ~equal =
  let rec helper acc = function
    | EVar s -> s :: acc
    | EConst _ -> []
    | ELet (_, _, e1, e2) -> helper (helper acc e1) e2
    | EAbs (v, l) -> acc @ list_remove ~equal v (helper [] l)
    | EApp (l, r) -> helper (helper acc r) l
  in
  helper []
;;

let is_free_in x term =
  List.mem (free_vars term ~equal:String.equal) x ~equal:String.equal
;;
