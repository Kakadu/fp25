[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type op =
  | Add (** e1 + e2 *)
  | Sub (** e1 - e2 *)
  | Mul (** e1 * e2 *)
  | Div (** e1 / e2 *)
  | Less (** e1 < e2 *)
  | Great (** e1 > e2 *)
  | LessEq (** e1 <= e2 *)
  | GreatEq (** e1 >= e2 *)
  | Equal (** e1 = e2 *)
  | NeqPhysical (** e1 != e2 *)
  | NeqStruct (** e1 <> e2 *)
  | And (** e1 && e2 *)
  | Or (** e1 || e2 *)

type instr =
  | Access of int (** Access variable *)
  | Cur of instr list (** Non-tail recursive closure *)
  | Const of int (** Put a constant to accumulator *)
  | Primitive of op (** Primitive operators *)
  | BranchIf of int (** Jump to specific offset if acc != 0 *)
  | Branch of int (** Unconditional jump*)
  | EndLet (** End of let "frame" *)
  | Return (** Return value and get to previous enviroment *)
  | Grab (** Get argument from argument stack *)
  | Apply (** Apply argument from accumulator *)
  | Let (** Start let "frame" *)
  | Push (** Push argument onto argument stack *)
  | PushMark (** Same as Push instruction but used first*)
  | AppTerm (** Tail-recursive version of EndLet *)
  | Dummy (** Push dummy symbol to enviroment *)
  | Update (** Replace the top of the enviroment stack  *)
  | Print (** Prints value in accumulator *)

val compile : Ast.brujin Ast.t -> instr list
val pp_instr : Format.formatter -> instr -> unit
val list_of_apps : 'a Ast.t -> 'a Ast.t list
