[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type prim =
  | Print_int
  | Fix

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

and env = (name * value) list

type error =
  | Unknown_variable of name
  | Not_a_function of value
  | Div_by_zero
  | Type_error of string
  | Out_of_fuel

type 'a eval_result = ('a, error) result

val ok : 'a -> 'a eval_result
val error : error -> 'a eval_result
val ( let* ) : 'a eval_result -> ('a -> 'b eval_result) -> 'b eval_result

type fuel = int

val tick : fuel -> (unit * fuel, error) result
val eval : env -> fuel -> expr -> (value * fuel, error) result
val initial_env : env
val string_of_value : value -> string
val string_of_error : error -> string

type run_error =
  | RuntimeError of error
  | ParseError of Parser.error

val string_of_run_error : run_error -> string
val run_program : ?fuel:fuel -> string -> (value * fuel, run_error) result
val parse_and_run : ?fuel:fuel -> string -> unit
