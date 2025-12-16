[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)

open Base
open Ast

(** модульный тип монады с ошибкой, вычисление дает 'a или ошибку 'e*)
module type MONAD_FAIL = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
end

(** Тип ошибок интерпритатора.*)
type error = 
  [`UnknownVariable of string (** Переменная не найдена в окружении*)
  | `IfConditionNotInt (** Условие if не является целым числом*)
  | `BinopOnNonInt (** Бинарная операция применена не к двум int*)
  | `NotAFunction (** Попытка вызвать не функцию*)
  | `DivisionByZero (** Деление на ноль*)
  | `ResultNotInt (** В конце получили не int *)
  | `FixOnNonFunction (** fix применен к не функции *)
  ]

  (** Функтор интерпретатора.
   На вход получает модуль, реализующий монаду с ошибкой,
   а на выходе дает модуль с функцией, которая интерпретирует AST и возвращает
   либо число, либо ошибку.
  *)

  module Interpret (M: MONAD_FAIL) : sig
    val run : Ast.name Ast.t -> (int, [> error]) M.t
  end = struct
    (** Результаты вычисления выражений*)
    type value = 
      |VInt of int
      (** Целое число времени исполнения*)
      |VClosure of name * name Ast.t * env
      (** Замыкание функции во время исполнения, name, имя параметра (name Ast.t) тело функции, env окружение в котором была определена*)
     (** Окружение в виде пар (имя перем и знач)*)
      and env = (name * value) list

(** Оборачиваем знач в монаду*)
      let return (x : 'a) : ('a, 'e) M.t =
        M.return x

        let ( let* ) (m : ('a, 'e) M.t) (f : 'a -> ('b, 'e) M.t) : ('b, 'e) M.t =
          M.bind m ~f
          (** завершить вычесление с ошибкой е*)
          let fail (e : 'e) : ('a, 'e) M.t =
            M.fail e
          (** Поиск переменной в окружении
          env,
          x - искомое имя,
          Если нашли возвращаем ок, если не ошибку UnknownVariable*)
          let rec lookup (env : env) (x : name) : (value, [> error]) M.t =
            match env with
            | [] ->
               fail (`UnknownVariable x)
            | (y,v) :: rest ->
               if String.equal x y then
               return v
               else
               lookup rest x
            (** Вычисление бинарной операции*)
            let eval_binop (op : binop) (n1 : int) (n2 : int)
            : (value, [> error]) M.t = match op with
            |Add -> return (VInt (n1 + n2))
            |Sub -> return (VInt (n1 - n2))
            |Mul -> return (VInt (n1 * n2))
            |Div -> if n2 = 0 then fail `DivisionByZero else return (VInt (n1 / n2))
            |Eq -> if Int.equal n1 n2 then return (VInt 1) else return (VInt 0)
            |Lt -> if n1 < n2 then return (VInt 1) else return (VInt 0)
            |Gt -> if n1 >n2 then return (VInt 1) else return (VInt 0)
            (** функция интерпретации 
            env,
            miniml,
            возвращает значение или ошибку*)
            let rec eval (env : env) (expr : name Ast.t) : (value, [> error]) M.t =
              match expr with
              |Var x -> lookup env x
              |Int n -> return (VInt n)
              |Abs (param, body) ->
              (** сторим замыкание из парам, тела и текущ окружения*)
              return (VClosure (param, body, env))
              |App (f_expr, arg_expr) ->
              (** сначала сичаем аргумент, потом подставляем*)
              let* f_val = eval env f_expr in
              let* arg_val = eval env arg_expr in
              begin
                match f_val with
                |VClosure (param, body, closure_env) ->
                (** новое окружение = окружение определения + (param -< arg_val)*)
                let env' : env = (param, arg_val) :: closure_env in
                eval env' body
                |VInt _ -> 
                (** попытка вызвать что не является функцией*)
                fail `NotAFunction
              end
              |Let (x, e1, e2) ->
              (** вычисляем е1, добавляем х -> знач в окружение, вычисляем е2 в новом окруж*)
              let* v1 = eval env e1 in
              let env' = (x, v1) :: env in
              eval env' e2
              |Let_rec (f, x, body, in_e) -> 
              (** делаем самоссылочное окружение, внутри тела функции она сама доступна под именем*)
              let rec env' : env = (f, VClosure (x, body, env')) :: env in eval env' in_e
                |If (cond, e_then, e_else) ->
                (** сначала считаем cond, ожидаем что инт = 0, все ост 1*)
                let* vcond = eval env cond in
                begin
                  match vcond with
                  |VInt n -> if n = 0 then eval env e_else else eval env e_then
                  |VClosure _ -> fail `IfConditionNotInt
                end
                |Binop (op, e1, e2) ->
                let* v1 = eval env e1 in
                let* v2 = eval env e2 in
                begin 
                  match v1, v2 with
                  |VInt n1, VInt n2 -> eval_binop op n1 n2
                  | _ -> fail `BinopOnNonInt
                end
                |Fix e1 ->
                (** ожидаем на входе функцию (замыкание), из нее строим рекурсивную функцию*)
                let* v = eval env e1 in
                begin
                  match v with
                  |VClosure (param, body, closure_env) ->
                  (** vfix это замыкание, которое внутри тела видит само себя*)
                  let rec vfix : value = VClosure (param, body, (param, vfix) :: closure_env)
                  in
                  return vfix
                  |VInt _ -> fail `FixOnNonFunction
                end
                let run (expr : name Ast.t) : (int, [> error]) M.t =
                  let* v = eval [] expr in
                  match v with
                  |VInt n -> return n
                  |VClosure _ -> fail `ResultNotInt
                end
                (** парсит строку, запускает интерпретатор и печатает результат*)
                let parse_and_run str = let module I = Interpret (Base.Result) in
                (** сначала парсим строку в ast, потом передаем его в интерпретатор*)
                let rez = Base.Result.(Parser.parse str >>= I.run) in
                match rez with
                | Result.Ok n -> Stdlib.Printf.printf "Success: %d\n" n
                |Result.Error #Parser.error -> Format.eprintf "Parsing error\n%!"; exit 1
                |Result.Error #error -> Format.eprintf "Interpreter error\n%!"; exit 1
                ;;