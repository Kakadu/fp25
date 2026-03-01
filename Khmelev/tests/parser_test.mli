(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Тесты для парсера miniML *)

(** Проверяет, что входная строка парсится в ожидаемое AST *)
val test_parse : string -> Ast.expr -> bool

(** Проверяет, что входная строка вызывает ошибку парсинга *)
val test_parse_error : string -> bool
