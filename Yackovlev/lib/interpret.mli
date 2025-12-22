[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(** Built in primitives of miniML *)
type prim =
  | Print_int
  (** [print_int : int -> unit] prints integer to stdout and returns [unit]. *)
  | Fix (** [fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b] fixed-point combinator *)

type value =
  | VInt of int
  | VClosure of closure
  | VPrim of prim
  | VUnit

(** Function closure with captured environment *)
and closure =
  { param : name
  ; body : expr
  ; env : env
  }

(** Environment as name-value list, later bindings shadow earlier *)
and env = (name * value) list

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

(** Consume one fuel step or return [`Out_of_fuel] *)
val tick : fuel -> (unit * fuel, error) result

(** Evaluate [e] in [env] with [fuel], return value and remaining fuel *)
val eval : env -> fuel -> expr -> (value * fuel, error) result

(** Initial environment with standard primitives. *)
val initial_env : env

(** Human-readable representation of runtime values. *)
val string_of_value : value -> string

(** Human-readable representation of runtime errors. *)
val string_of_error : error -> string

(** Errors that can happen when running a whole program:
    either a parsing error or a runtime error. *)
type run_error =
  [ error
  | Parser.error
  ]

(** Human-readable representation of [run_error]. *)
val string_of_run_error : run_error -> string

(** [run_program ?fuel src] parses [src], evaluates it in [initial_env]
    with given [fuel] (default: [100_000]) and returns either
    [(value, remaining_fuel)] or a [run_error]. *)
val run_program : ?fuel:fuel -> string -> (value * fuel, run_error) result

(** Convenience wrapper: run program from string and print result or error. *)
val parse_and_run : ?fuel:fuel -> string -> unit
