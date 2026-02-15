[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Ошибки интерпретатора (полиморфные варианты). *)
type error =
  [ `UnknownVariable of string
  | `IfConditionNotInt
  | `BinopOnNonInt
  | `NotAFunction
  | `DivisionByZero
  | `ResultNotInt
  | `FixOnNonFunction
  | `PrintArgumentNotInt
  ]

(** Чистый запуск: парсит строку и возвращает либо (результат, stdout-лог),
    либо ошибку парсера, либо ошибку интерпретатора. *)
val run_string : string -> (int * string list, [> Parser.error | error ]) result
