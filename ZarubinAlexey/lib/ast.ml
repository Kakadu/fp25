[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string

(** Бинарные операции miniML*)
type binop =
  | Add (** [e1 + e2] сложение целых *)
  | Sub (** [e1 - e2] вычитание *)
  | Mul (** [e1 * e2] умножение *)
  | Div (** [e1 / e2] целочисленное деление, ошибка при делении на 0 *)
  | Eq (** [e1 = e2] равно: результат 1, если равны, иначе 0 *)
  | Lt (** [e1 < e2] меньше: результат 1, если [e1 < e2], иначе 0 *)
  | Gt (** [e1 > e2] больше: результат 1, если [e1 > e2], иначе 0 *)
[@@deriving show { with_path = false }]

(* Главное дерево абстрактного синтаксиса *)
type 'name t =
  | Var of 'name (** Переменная [x]. Примеры: [x], [n], [fact]. *)
  | Abs of 'name * 'name t (** Анонимная функция [fun x -> body]*)
  | App of 'name t * 'name t (** Применение функции [f a]*)
  | Int of int (** Целое число. Примеры: [0], [1], [42], [-5]. *)
  | Let of 'name * 'name t * 'name t
  (** Нерекурсивное определение:
      [let x = e1 in e2]
      Сначала вычисляем [e1] , результат связываем с именем [x],
      потом вычисляем [e2] в окружении *)
  | Let_rec of 'name * 'name * 'name t * 'name t
  (** Рекурсивное определение функции:
      [let rec f x = body in e] *)
  | If of 'name t * 'name t * 'name t
  (** [If (cond, e1, e2)] условное выражение: если [cond] не равно нулю вычислить [e1], иначе [e2] *)
  | Binop of binop * 'name t * 'name t
  (** [Binop (add, sub, div)] бинарная операция над двумя выражениями *)
  | Fix of 'name t (** Оператор фиксированной точки [fix f] *)
