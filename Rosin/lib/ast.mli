[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type varname = string

(** integer binary operations *)
type unop =
  | Inc (** incrementing value [++x] *)
  | Dec (** decrementing value [--x]*)

(** integer binary operations *)
type binop =
  | Plus (** addition of two arguments [x + y] *)
  | Minus (** subtraction of two arguments [x - y] *)
  | Mult (** mulitplication of two arguments [x * y] *)
  | Div (** division of two arguments [x / y] *)

(** The main type for AST *)
type expr =
  | Num of int (** integer constant *)
  | Var of varname (** variable *)
  | Binop of binop * expr * expr (** binary operation *)
  | If of expr * expr * expr option (** if-then-(else) *)
  | Fun of varname * expr (** function definitions *)
  | Let of varname * expr * expr (** local naming [let varname = expr in expr] *)
  | Letrec of varname * expr * expr
  (** recursive local naming [letrec varname = expr in expr] *)
  | App of expr * expr (** function applications *)
