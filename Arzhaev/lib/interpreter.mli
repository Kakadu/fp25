open Ast

type state = int

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VClosure of closure
  | VUnit

and closure =
  { param : string
  ; body : expr
  ; env : value Utils.Table.t
  ; label : reclabel
  }

and reclabel =
  | Nonrec
  | Rec of string
[@@deriving show]

type runtime_error =
  | RUnboundValue of string
  | RNotAFunction of value
  | RDivisionByZero
  | RTypeMismatch
  | RInvalidOperand
  | RIfCondNotBool
  | RStepLimitExceeded
  | REvalError

type 'a evalres =
  | Failed of runtime_error
  | Ok of state * 'a

type toplevel_value =
  | VLet of string * value
  | VExpr of value

val pp_value : Format.formatter -> value -> unit
val pp_toplevel_value : Format.formatter -> toplevel_value -> unit

val run_eval
  :  toplevel
  -> value Utils.Table.t
  -> state
  -> (value Utils.Table.t * toplevel_value) evalres

val print_result : value evalres -> unit
val pp_runtime_error : Format.formatter -> runtime_error -> unit
