[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)

open Base
open Ast

(** модульный тип монады с ошибкой, вычисление дает 'a или ошибку 'e *)
module type MONAD_FAIL = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
end

(** Тип ошибок интерпретатора. *)
type error =
  [ `Parsing_error of string
  | `UnknownVariable of string (** Переменная не найдена в окружении *)
  | `IfConditionNotInt (** Условие if не является целым числом *)
  | `BinopOnNonInt (** Бинарная операция применена не к двум int *)
  | `NotAFunction (** Попытка вызвать не функцию *)
  | `DivisionByZero (** Деление на ноль *)
  | `ResultNotInt (** В конце получили не int *)
  | `FixOnNonFunction (** fix применен к не функции *)
  | `PrintArgumentNotInt (** print получил не int *)
  | `StepLimitExceeded (** Превышен лимит шагов интерпретатора *)
  ]

(** Функтор интерпретатора.
    На вход получает модуль, реализующий монаду с ошибкой,
    а на выходе дает модуль с функцией, которая интерпретирует AST и возвращает
    либо число + лог stdout, либо ошибку. *)
module Interpret (M : MONAD_FAIL) : sig
  (** [run expr] интерпретирует выражение [expr] и возвращает
      пару [(n, out)], где [n] — итоговое целое значение программы,
      а [out] — список строк, напечатанных встроенной функцией [print]
      в порядке их появления. *)
  val run : steps:int -> Ast.name Ast.t -> (int * string list, error) M.t
end = struct
  (** Результаты вычисления выражений. *)
  type value =
    | VInt of int (** Целое число времени исполнения. *)
    | VClosure of name * name Ast.t * env
    (** Замыкание функции: имя параметра, тело и окружение определения. *)
    | VBuiltinPrint (** Встроенная функция [print]. *)

  (** Окружение в виде списка пар [(имя переменной, значение)]. *)
  and env = (name * value) list

  (** Обертка над базовой монадай M, которая добавляет лог строк stdout
      И лимит шагов (fuel).
      Каждое вычисление получает [steps] и возвращает:
      значение,
      оставшиеся steps,
      и список строк (вывод программы). *)

  (** Функция, которая по лимиту шагов возвращает монадическое вычисление,
      которое либо выдаст результат 'a + (инфо про шаги) + список напечатанных строк, либо ошибку *)
  type 'a eval = int -> (('a * int) * string list, error) M.t

  let return (x : 'a) : 'a eval = fun steps -> M.return ((x, steps), [])

  (* Связывание (bind) для нашего типа вычислений ['a eval].
     Объединяет два вычисления в одно:
     запускает [m] с текущим лимитом шагов [steps0];
     берёт результат [v] и обновлённый лимит [steps1], затем запускает [f v] уже с [steps1];
     склеивает накопленный вывод в правильном порядке: [out1 @ out2];
     автоматически прокидывает ошибки через [M.bind]: если падает [m] или [f v], падает всё вычисление. *)
  let ( let* ) (m : 'a eval) (f : 'a -> 'b eval) : 'b eval =
    fun steps0 ->
    M.bind (m steps0) ~f:(fun ((v, steps1), out1) ->
      M.bind (f v steps1) ~f:(fun ((res, steps2), out2) ->
        M.return ((res, steps2), out1 @ out2)))
  ;;

  (* Создаёт вычисление интерпретатора, которое всегда завершается ошибкой [e].
     Лимит шагов не используется, вывод не добавляется ошибка сразу поднимается в монаду [M]. *)
  let fail (e : [> error ]) : 'a eval = fun _steps -> M.fail e

  (** Добавить строку в лог "stdout". *)
  let tell (s : string) : unit eval = fun steps -> M.return (((), steps), [ s ])

  (** Списать один шаг. Если шагов не осталось прекращаем вычисление. *)
  let tick : unit eval =
    fun steps ->
    if steps <= 0 then M.fail `StepLimitExceeded else M.return (((), steps - 1), [])
  ;;

  (** Поиск переменной в окружении.
      Если переменная найдена возвращаем её значение,
      иначе ошибку [`UnknownVariable]. *)
  let rec lookup (env : env) (x : name) =
    match env with
    | [] -> fail (`UnknownVariable x)
    | (y, v) :: rest -> if String.equal x y then return v else lookup rest x
  ;;

  (** Вычисление бинарной операции над целыми числами. *)
  let eval_binop (op : binop) (n1 : int) (n2 : int) =
    match op with
    | Add -> return (VInt (n1 + n2))
    | Sub -> return (VInt (n1 - n2))
    | Mul -> return (VInt (n1 * n2))
    | Div -> if n2 = 0 then fail `DivisionByZero else return (VInt (n1 / n2))
    | Eq -> if Int.equal n1 n2 then return (VInt 1) else return (VInt 0)
    | Lt -> if n1 < n2 then return (VInt 1) else return (VInt 0)
    | Gt -> if n1 > n2 then return (VInt 1) else return (VInt 0)
  ;;

  (** Разворачивание [fix e] через [let rec].
      В простейшем варианте:
      [fix e] --> [let rec f x = e f x in f]*)
  let desugar_fix (e : name Ast.t) : name Ast.t =
    let f = "_fix_f" in
    let x = "_fix_x" in
    let body = App (App (e, Var f), Var x) in
    Let_rec (f, x, body, Var f)
  ;;

  (** Начальное окружение: в нем сразу есть встроенная функция [print]. *)
  let initial_env : env = [ "print", VBuiltinPrint ]

  (** Основная функция интерпретации. *)
  let rec eval (env : env) (expr : name Ast.t) : value eval =
    let* () = tick in
    match expr with
    | Var x -> lookup env x
    | Int n -> return (VInt n)
    | Abs (param, body) ->
      (* строим замыкание из параметра, тела и текущего окружения *)
      return (VClosure (param, body, env))
    | App (f_expr, arg_expr) ->
      (* сначала считаем функцию и аргумент *)
      let* f_val = eval env f_expr in
      let* arg_val = eval env arg_expr in
      (match f_val with
       | VClosure (param, body, closure_env) ->
         let env' : env = (param, arg_val) :: closure_env in
         eval env' body
       | VBuiltinPrint ->
         (* встроенный print: пока печатаем только int *)
         (match arg_val with
          | VInt n ->
            let* () = tell (Int.to_string n) in
            (* пусть print возвращает 0 *)
            return (VInt 0)
          | VClosure _ | VBuiltinPrint -> fail `PrintArgumentNotInt)
       | VInt _ -> fail `NotAFunction)
    | Let (x, e1, e2) ->
      let* v1 = eval env e1 in
      let env' = (x, v1) :: env in
      eval env' e2
    | Let_rec (f, x, body, in_e) ->
      (* самоссылочное окружение: внутри [body] имя [f] видно как сама функция *)
      let rec env' : env = (f, VClosure (x, body, env')) :: env in
      eval env' in_e
    | If (cond, e_then, e_else) ->
      let* vcond = eval env cond in
      (match vcond with
       | VInt n -> if n = 0 then eval env e_else else eval env e_then
       | VClosure _ | VBuiltinPrint -> fail `IfConditionNotInt)
    | Binop (op, e1, e2) ->
      let* v1 = eval env e1 in
      let* v2 = eval env e2 in
      (match v1, v2 with
       | VInt n1, VInt n2 -> eval_binop op n1 n2
       | _ -> fail `BinopOnNonInt)
    | Fix e1 ->
      (* вместо отдельной логики просто разворачиваем [fix] в [let rec] *)
      eval env (desugar_fix e1)
  ;;

  (** Запуск интерпретатора в исходном окружении:
      вычисляем выражение [expr] в начальном окружении [initial_env] с лимитом шагов [steps],
      собираем накопленный вывод (например, от [print]) и проверяем, что итоговое значение целое.
      Если результат не [VInt], возвращаем ошибку [`ResultNotInt]. *)
  let run ~steps (expr : name Ast.t) : (int * string list, [> error ]) M.t =
    M.bind (eval initial_env expr steps) ~f:(fun ((v, _steps_left), out) ->
      match v with
      | VInt n -> M.return (n, out)
      | VClosure _ | VBuiltinPrint -> M.fail `ResultNotInt)
  ;;
end

(** Чистый запуск без печати.
    Возвращает либо (число, лог print), либо ошибку парсера/интерпретатора. *)
let run_string_with_steps ~(steps : int) (str : string)
  : (int * string list, error) result
  =
  let module I = Interpret (Base.Result) in
  match Parser.parse str with
  | Ok ast -> I.run ~steps ast
  | Error (`Parsing_error msg) -> Error (`Parsing_error msg)
;;

let run_string (str : string) : (int * string list, error) result =
  run_string_with_steps ~steps:10_000 str
;;
