[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Name of variable or function *)
type name = string

(* Binary arithmetic operations *)
type binop =
  | Add  (* e1 + e2 *)
  | Sub  (* e1 - e2 *)
  | Mul  (* e1 * e2 *)
  | Div  (* e1 / e2 *)

(* Comparison operations *)
type cmpop =
  | Eq   (* e1 = e2 *)
  | Neq  (* e1 <> e2 *)
  | Lt   (* e1 < e2 *)
  | Le   (* e1 <= e2 *)
  | Gt   (* e1 > e2 *)
  | Ge   (* e1 >= e2 *)

(* AST for miniML expressions *)
type 'name t =
  | Var of 'name
  (* Variable, like x *)

  | Int of int
  (* Integer literal *)

  | Abs of 'name * 'name t
  (* Function: fun x -> e *)

  | App of 'name t * 'name t
  (* Function application: e1 e2 *)

  | Let of 'name * 'name t * 'name t
  (* Non-recursive let: let x = e1 in e2 *)

  | Let_rec of 'name * 'name t * 'name t
  (* Recursive let: let rec f = e1 in e2 *)

  | If of 'name t * 'name t * 'name t
  (* If expression *)

  | Binop of binop * 'name t * 'name t
  (* Arithmetic operation *)

  | Cmp of cmpop * 'name t * 'name t
  (* Comparison *)

(* Expression with string names *)
type expr = name t

(* Whole program is one expression *)
type program = expr
