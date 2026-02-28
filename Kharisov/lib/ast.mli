[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* identifier *)
type id = string

(* binary operator *)
type binop =
  | Add (** [+] *)
  | Sub (** [-] *)
  | Mul (** [*] *)
  | Div (** [/] *)
  | Eq (** [=] *)
  | Neq (** [<>] *)
  | Lt (** [<] *)
  | Le (** [<=] *)
  | Gt (** [>] *)
  | Ge (** [>=] *)

(* recursion flag for let bindings *)
type rec_flag =
  | Rec (** [let rec] *)
  | NonRec (** [let] *)

(* expression *)
type expr =
  | EConst of int (** integer literal: [42], [-1] *)
  | EVar of id (** variable: [x], [fact] *)
  | EBinop of binop * expr * expr (** binary operation: [e1 + e2] *)
  | EIf of expr * expr * expr (** conditional: [if c then e1 else e2] *)
  | EFun of id * expr (** abstraction: [fun x -> e] *)
  | EApp of expr * expr (** application: [f x] *)
  | ELet of rec_flag * id * expr * expr (** binding: [let (rec)? x = e1 in e2] *)
