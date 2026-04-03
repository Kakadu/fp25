[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type op = 
| Add (** Addition operator [+] *)
| Sub (** Subtraction operator [-] *)
| Mul (** Multiplication operator [*] *)
| Div (** Division operator [/] *)
| Lt (** Less-than comparison [<] *)
| Eq (** Equality comparison [=] *)
| Mt (** Greater-than comparison [>] *)

type rec_flag =
| Rec (** Recursive [let rec] binding *)
| Val (** Non-recursive [let] binding *)

type  expr = 
| Const of int (** Integer literal *)
| BinOp of op * expr * expr (** Binary operation with two operands *)
| Var of string (** Variable *)
| Let of  rec_flag * string * expr * expr (** [let] or [let rec] binding *)
| If of expr * expr * expr (** Conditional [if then else] expression *)
| App of expr * expr (** Function application *)
| Fun of string * expr (** Lambda abstraction with one argument *)
| Neg of expr (** Unary minus *)
