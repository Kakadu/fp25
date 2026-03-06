(** Copyright 20265, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type tc_error =
  | NotImplemented
  | OccursCheck
  | AccessError
  | ImpossibleResult of string
  | TypeMismatch
  | OtherError of string
[@@deriving show { with_path = false }]

type interpret_error =
  | NotImplemented
  | NoVariable of string
  | AddressNotFound of int
  | VarDeclared of string
  | TypeMismatch
  | ImpossibleResult of string
  | OtherError of string
[@@deriving show { with_path = false }]

type error =
  | TCError of tc_error
  | IError of interpret_error
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

type var_info =
  { var_type : var_type
  ; initialized : bool (* TODO: ?? *)
  }
[@@deriving show { with_path = false }, eq]

type field_info =
  { field_modifiers : modifier list
  ; field_type : var_type
  ; field_name : ident
  ; field_init : expr option
  ; is_static : bool
  }
[@@deriving show { with_path = false }, eq]

type method_info =
  { method_modifiers : modifier list
  ; method_return : _type
  ; method_name : ident
  ; method_params : params
  ; method_body : stmt
  ; is_static : bool
  ; is_main : bool
  }
[@@deriving show { with_path = false }, eq]

type obj_content =
  | TCLocalVar of var_info
  | TCField of field_info
  | TCMethod of method_info
[@@deriving show { with_path = false }, eq]

type context = TCClass of c_sharp_class

module TypeCheck = struct
  type global_env = context IdMap.t
  type local_env = obj_content IdMap.t
  type curr_class = ident
  type class_with_main = ident

  type state =
    global_env * local_env * curr_class option * _type option * class_with_main option
end
