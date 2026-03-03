(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Тесты для интерпретатора miniML *)

(** Проверяет, что выражение вычисляется в ожидаемое целое число *)
val test_eval : Ast.expr -> int -> bool

(** Проверяет, что выражение завершается с ожидаемой ошибкой *)
val test_eval_error : Ast.expr -> Interpret.error -> bool
