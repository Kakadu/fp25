(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type interpret_error =
  | NotImplemented
  | NoVariable of string
  | AddressNotFound of int
  | VarDeclared of string
  | TypeMismatch
  | ImpossibleResult of string
  | OtherError of string

type error = IError of interpret_error

val pp_error : Format.formatter -> error -> unit
val show_error : error -> string

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

(* Main funtions *)
val interpret_program : program -> (value option, interpret_error) result
val interpret : string -> (value option, interpret_error) result
