(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(* Type checker error types *)
type tc_error =
  | NotImplemented
  | OccursCheck
  | AccessError
  | ImpossibleResult of string
  | TypeMismatch
  | OtherError of string

val pp_tc_error : Format.formatter -> tc_error -> unit
val show_tc_error : tc_error -> string

(* Interpreter error types *)
type interpret_error =
  | NotImplemented
  | NoVariable of string
  | AddressNotFound of int
  | VarDeclared of string
  | TypeMismatch
  | ImpossibleResult of string
  | OtherError of string

val pp_interpret_error : Format.formatter -> interpret_error -> unit
val show_interpret_error : interpret_error -> string

(* Combined error type *)
type error =
  | TCError of tc_error
  | IError of interpret_error

val pp_error : Format.formatter -> error -> unit
val show_error : error -> string

(* Identifier module *)
module Id : sig
  type t = ident

  val compare : t -> t -> int
end

(* Map from identifiers *)
module IdMap : sig
  include Map.S with type key = ident
end

(* Address type *)
type adr = Adr of int

val pp_adr : Format.formatter -> adr -> unit
val show_adr : adr -> string

(* Address module *)
module Adr : sig
  type t = adr

  val compare : t -> t -> int
end

(* Map from addresses *)
module AdrMap : sig
  include Map.S with type key = adr
end

(* Variable information for type checker *)
type tc_var_info =
  { var_type : var_type
  ; initialized : bool (* Whether the variable has been initialized *)
  }

val pp_tc_var_info : Format.formatter -> tc_var_info -> unit
val show_tc_var_info : tc_var_info -> string
val equal_tc_var_info : tc_var_info -> tc_var_info -> bool

(* Field information for type checker *)
type field_info =
  { field_modifiers : modifier list
  ; field_type : var_type
  ; field_name : ident
  ; field_init : expr option
  ; is_static : bool
  }

(* Method information for type checker *)
type method_info =
  { method_modifiers : modifier list
  ; method_return : _type
  ; method_name : ident
  ; method_params : params
  ; method_body : stmt
  ; is_static : bool
  ; is_main : bool (* Whether this is the Main method *)
  }

(* Type checker content types *)
type obj_content =
  | TCLocalVar of tc_var_info (* Local variable *)
  | TCField of field_info (* Class field *)
  | TCMethod of method_info (* Class method *)

(* Global context for type checker *)
type context = TCClass of c_sharp_class

(* Type checker state module *)
module TypeCheck : sig
  type global_env = context IdMap.t
  type local_env = obj_content IdMap.t
  type curr_class = ident
  type class_with_main = ident

  type state =
    global_env * local_env * curr_class option * _type option * class_with_main option
end
