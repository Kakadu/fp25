[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type constant =
  | CInt of int
  | CBool of bool
  | CUnit

type pattern =
  | PVar of string
  | PAny

type rec_flag =
  | NonRec
  | Rec

type binop =
  | Plus
  | Minus
  | Asteriks
  | Dash
  | Equals
  | MoreThan
  | LessThan
  | EqLess
  | EqMore

type expr =
  | Constant of constant
  | Var of pattern
  | Let of rec_flag * pattern * expr * expr option
  | Fun of pattern * expr
  | App of expr * expr
  | Binop of binop * expr * expr
  | Conditional of expr * expr * expr option

(*TODO
  - обрабатывать _ как отдельный случай
  - распознавать переменные внутри выражений

  - распознавать функции
*)
