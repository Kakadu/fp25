[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string [@@deriving show { with_path = false }]

(** Бинарные операторы для арифметики и сравнения *)
type binop =
  | Add (** Сложение: '+' *)
  | Sub (** Вычитание: '-' *)
  | Mul (** Умножение: '*' *)
  | Div (** Деление: '/' *)
  | Eq (** Равенство: '=' *)
  | Lt (** Меньше: '<' *)
  | Gt (** Больше: '>' *)
  | Le (** Меньше или равно: '<=' *)
  | Ge (** Больше или равно: '>=' *)
[@@deriving show { with_path = false }]

type expr =
  | Var of name (** Переменная, например: `x`, `y`, `foo` *)
  | Abs of name * expr (** Лямбда-функция (абстракция), например: `fun x -> x + 1` *)
  | App of expr * expr (** Применение функции, например: `f 5` *)
  | Const of int (** Целочисленная константа, например: `0`, `15`, `-7` *)
  | BinOp of binop * expr * expr (** Бинарная операция, например: `2 + 3`, `x * y` *)
  | If of expr * expr * expr
  (** Условное выражение: `if условие then ветка1 else ветка2` *)
  | Let of name * expr * expr (** Let-связывание: `let x = значение in тело` *)
  | LetRec of name * name * expr * expr
  (** Рекурсивное let-связывание: `let rec f x = тело_f in выражение` *)
  | Fix of expr (** Оператор фиксированной точки для рекурсии (fix combinator) *)
  | Prim of string * expr list (** Примитивные функции: `println_int(42)` *)
[@@deriving show { with_path = false }]
