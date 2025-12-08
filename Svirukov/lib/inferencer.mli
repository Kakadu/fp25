(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | TInt
  | TFun of typ * typ
  | TUnit
  | TVar of string

type type_env = (string, typ, Base.String.comparator_witness) Base.Map.t

type type_error =
  | UnboundVar of string
  | TypeMismatch of typ * typ
  | OccursCheckError
  | InvalidCondition
  | ApplicationError
  | NotExpression

val typecheck_program : Ast.expr -> (typ, type_error) result
