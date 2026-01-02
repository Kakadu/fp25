[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type typ =
  | TInt
  | TBool
  | TUnit
  | TFun of typ * typ
  | TTuple of typ list
  | TVar of tvar ref

and tvar =
  | Unbound of int
  | Link of typ

(* Полиморфная схема типов (нужна для сигнатуры infer) *)
type scheme = Forall of int list * typ

(* Исключение для ошибок типов *)
exception TypeError of string

(* Основная функция вывода типов *)
val infer : (string * scheme) list -> expr -> typ
