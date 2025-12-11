[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string

(** Бинарные операции miniML*)
type binop = 
  |Add
  |Sub
  |Mul
  |Div
  |Eq
  |Lt
  |Gt

(* Главное дерево абстрактного синтаксиса *)
type 'name t =
 | Var of 'name
 | Abs of 'name * 'name t 
 (** Анонимная функция [fun x -> body]*)
 | App of 'name t * 'name t 
 (** Применение функции [f a]*)
 | Int of int 
 | Let of 'name * 'name t * 'name t
    (** Нерекурсивное определение:
    [let x = e1 in e2]
    Сначала вычисляем [e1] , результат связываем с именем [x],
    потом вычисляем [e2] в окружении
    *)
 | Let_rec of 'name * 'name * 'name t * 'name t
 (** Рекурсивное определение функции:
  [let rec f x = body in e]
  *)
  | If of 'name t * 'name t * 'name t
  | Binop of binop * 'name t * 'name t
  | Fix of 'name t
  (** Оператор фиксированной точки
  [fix f]
  *)