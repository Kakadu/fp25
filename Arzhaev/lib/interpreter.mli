[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitry Arzhaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(** Execution state.
    Represents the remaining number of evaluation steps
    used to prevent non-termination. *)
type state = int

(** Runtime values produced by the interpreter. *)
type value =
  | VInt of int (** Integer value *)
  | VFloat of float (** Floating-point value *)
  | VBool of bool (** Boolean value *)
  | VClosure of closure (** Function closure *)
  | VUnit (** Unit value *)

(** A function closure capturing its environment. *)
and closure =
  { param : string (** Function parameter name *)
  ; body : expr (** Function body expression *)
  ; env : value Utils.Table.t (** Environment captured at definition time *)
  ; label : reclabel (** Recursion label *)
  }

(** Indicates whether a closure is recursive. *)
and reclabel =
  | Nonrec (** Non-recursive function *)
  | Rec of string (** Recursive function with its name *)
[@@deriving show]

(** Runtime errors that may occur during evaluation. *)
type runtime_error =
  | RUnboundValue of string (** Unbound variable access *)
  | RNotAFunction of value (** Attempt to apply a non-function value *)
  | RDivisionByZero (** Division by zero *)
  | RTypeMismatch (** General type mismatch *)
  | RInvalidOperand (** Invalid operand types *)
  | RIfCondNotBool (** If condition is not a boolean *)
  | RStepLimitExceeded (** Evaluation step limit exceeded *)
  | REvalError (** Generic evaluation error *)

(** Result of evaluation:
    either a runtime failure or a successful value with updated state. *)
type 'a evalres =
  | EFailed of runtime_error
  | EOk of state * 'a

(** Result of evaluating a toplevel phrase. *)
type toplevel_value =
  | VLet of string * value (** Result of a let binding *)
  | VExpr of value (** Result of an expression *)

(** Pretty-printer for runtime values. *)
val pp_value : Format.formatter -> value -> unit

(** Pretty-printer for toplevel evaluation results. *)
val pp_toplevel_value : Format.formatter -> toplevel_value -> unit

(** Evaluate a toplevel phrase.

    @param toplevel the phrase to evaluate
    @param env current runtime environment
    @param state evaluation step budget
    @return either runtime error or updated environment and result *)
val run_eval
  :  toplevel
  -> value Utils.Table.t
  -> state
  -> (value Utils.Table.t * toplevel_value) evalres

(** Pretty-printer for runtime errors. *)
val pp_runtime_error : Format.formatter -> runtime_error -> unit
