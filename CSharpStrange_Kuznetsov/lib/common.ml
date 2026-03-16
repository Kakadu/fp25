(** Copyright 20265, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** Type checking errors *)
type tc_error =
  | NotImplemented (** Feature not implemented in the type checker *)
  | OccursCheck
  (** Occurs check failed during type unification (typically for recursive types) *)
  | AccessError
  (** Invalid access to a member (e.g., accessing private member from outside) *)
  | ImpossibleResult of string
  (** Type checking encountered an impossible state with additional context *)
  | TypeMismatch (** Expected type does not match actual type *)
  | OtherError of string (** Other type checking error with description *)
[@@deriving show { with_path = false }]

(** Runtime interpretation errors *)
type interpret_error =
  | NotImplemented (** Feature not implemented in the interpreter *)
  | NoVariable of string (** Variable not found in current scope *)
  | AddressNotFound of int (** Memory address not found in store *)
  | VarDeclared of string (** Variable already declared in current scope *)
  | TypeMismatch (** Type mismatch during runtime operation *)
  | ImpossibleResult of string
  (** Interpreter encountered an impossible state with additional context *)
  | OtherError of string (** Other runtime error with description *)
[@@deriving show { with_path = false }]

(** Union type for all possible errors *)
type error =
  | TCError of tc_error (** Type checking error *)
  | IError of interpret_error (** Runtime interpretation error *)
[@@deriving show { with_path = false }]

module Id = struct
  type t = ident

  let compare = compare
end

module IdMap = Map.Make (Id)

type adr = Adr of int [@@deriving show { with_path = false }]

module Adr = struct
  type t = adr

  let compare = compare
end

module AdrMap = Map.Make (Adr)

type tc_var_info =
  { var_type : var_type
  ; initialized : bool
  }
[@@deriving show { with_path = false }, eq]

type field_info =
  { field_modifiers : modifier list
  ; field_type : var_type
  ; field_name : ident
  ; field_init : expr option
  ; is_static : bool
  }

type method_info =
  { method_modifiers : modifier list
  ; method_return : _type
  ; method_name : ident
  ; method_params : params
  ; method_body : stmt
  ; is_static : bool
  ; is_main : bool
  }

type obj_content =
  | TCLocalVar of tc_var_info
  | TCField of field_info
  | TCMethod of method_info

type context = TCClass of c_sharp_class

module TypeCheck = struct
  type global_env = context IdMap.t
  type local_env = obj_content IdMap.t
  type curr_class = ident
  type class_with_main = ident

  type state =
    global_env * local_env * curr_class option * _type option * class_with_main option
end
