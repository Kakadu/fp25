(** Copyright 2026, Kirill K. Smirnov *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type mlterm =
    | Var of string
    | Int of int
    | Bool of bool
    | Unit
    | ITE of mlterm * mlterm * mlterm
    | Let of string * mlterm * mlterm
    | LetRec of string * mlterm * mlterm
    | App of mlterm * mlterm
    | Fun of string * mlterm
