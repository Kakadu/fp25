[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type op = 
| Add
| Sub
| Mul
| Div
| Lt
| Eq
| Mt

type rec_flag =
| Rec
| Val

type  expr = 
| Const of int
| BinOp of op * expr * expr
| Var of string
| Let of  rec_flag * string * expr * expr
| If of expr * expr * expr
| App of expr * expr 
| Fun of string * expr 
| Neg of expr
