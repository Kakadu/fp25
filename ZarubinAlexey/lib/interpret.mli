[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Ошибки интерпретатора (полиморфные варианты). *)
type error =
  [ `Parsing_error of string
  | `UnknownVariable of string (** Переменная не найдена в окружении *)
  | `IfConditionNotInt
  | `BinopOnNonInt
  | `NotAFunction
  | `DivisionByZero
  | `ResultNotInt
  | `FixOnNonFunction
  | `PrintArgumentNotInt
  | `StepLimitExceeded
  ]

(** Чистый запуск: парсит строку и возвращает либо (результат, stdout-лог),
    либо ошибку парсера, либо ошибку интерпретатора. *)
val run_string : string -> (int * string list, error) result

(** Чистый запуск с явным лимитом шагов. *)
val run_string_with_steps : steps:int -> string -> (int * string list, error) result
