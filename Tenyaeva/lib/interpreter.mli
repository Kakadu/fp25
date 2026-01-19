(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type eval_error =
  | TypeError
  (** Represents a type error that occurs when a type mismatch is detected in an expression. *)
  | DivisionByZero
  (** Represents the error that occurs when attempting to perform a division by zero operation. *)
  | MatchFailure
  (** Represents a match error occurs when a pattern matching attempt fails. *)
  | NoVariable of Ast.ident
  (** Represents an error that occurs when attempting to use a variable that has not been declared or initialized. *)
  | OutOfSteps
  (** Represents an error that occurs when the permissible number of interpretation steps is exceeded. *)

type value =
  | ValInt of int
  | ValBool of bool
  | ValUnit
  | ValFun of Ast.rec_flag * Ast.pattern * Ast.expression * environment
  | ValFunction of Ast.case list * environment
  | ValOption of value option
  | ValBuiltin of Ast.ident

and environment = (Ast.ident, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit
val pp_eval_error : Format.formatter -> eval_error -> unit

val run_interpreter
  :  Ast.structure
  -> int
  -> ((Ast.ident option * value) list, eval_error) Result.t
