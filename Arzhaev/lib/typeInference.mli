open Ast
open Utils

type ground =
  | GInt
  | GBool
  | GFloat

type typ =
  | TGround of ground
  | TVar of string
  | TArrow of typ * typ

type scheme =
  { vars : string list
  ; ty : typ
  }

type subst = typ Table.t

type state =
  { sub : subst
  ; fresh : typ
  }

type infer_error =
  | IUnboundValue of string
  | IOccursCheck of string * typ
  | ITypeMismatch of typ * typ
  | ITypeError

val pp_infer_error : Format.formatter -> infer_error -> unit

type 'a infresult =
  | Failed of infer_error
  | Ok of state * 'a

type toplevel_result =
  | RLet of string * scheme
  | RExpr of typ

val pp_typ : Format.formatter -> typ -> unit
val pp_scheme : Format.formatter -> scheme -> unit
val pp_toplevel_result : Format.formatter -> toplevel_result -> unit

val run_infer
  :  toplevel
  -> scheme Utils.Table.t
  -> (scheme Utils.Table.t * toplevel_result) infresult
