(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common

type adr = Adr of int

type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VNull
  | VObject of adr

val pp_value : Format.formatter -> value -> unit
val show_value : value -> string

type return_code = int

(* Main funtions *)
val interpret_program : program -> (return_code option, error) result
val interpret : string -> (return_code option, error) result
