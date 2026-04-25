open Ast

type state = int

type 'a evalres =
  | Failed of string
  | Ok of state * 'a

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VClosure of closure
  | VUnit

and closure

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
