[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(** Built in primitives of miniML *)
type prim =
  | Print_int
  (** [print_int : int -> unit] *)
  | Fix
  (** [fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b] fixed-point combinator *)

type value =
  | VInt of int
  | VClosure of closure
  | VPrim of prim
  | VUnit

and closure =
  { param : name
  ; body : expr
  ; env : env
  }
  (** Function closure with captured environment *)

and env = (name * value) list
(** Environment as name-value list, later bindings shadow earlier *)

(** Runtime errors, extensible variant *)
type error =
  [ `Unknown_variable of name
  | `Not_a_function of value
  | `Division_by_zero
  | `Type_error of string
  | `Out_of_fuel
  ]

type 'a eval_result = ('a, error) result

val ok : 'a -> 'a eval_result

val error : error -> 'a eval_result

val ( let* ) : 'a eval_result -> ('a -> 'b eval_result) -> 'b eval_result

(** Fuel limits interpreter steps *)
type fuel = int

val tick : fuel -> (unit * fuel, error) result
(** Consume one fuel step or return [`Out_of_fuel] *)

val eval : env -> fuel -> expr -> ((value * fuel), error) result
(** Evaluate [e] in [env] with [fuel], return value and remaining fuel *)

val initial_env : env

val parse_and_run : ?fuel:fuel -> string -> unit
(** Parse, run in [initial_env], print result or error *)
