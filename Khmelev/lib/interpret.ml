[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base

type error =
  [ `UnknownVariable of string
  | `TypeError of string
  | `DivisionByZero
  | `StepLimitReached
  ]

let pp_error ppf = function
  | `UnknownVariable s -> Format.fprintf ppf "Unknown variable: %s" s
  | `TypeError s -> Format.fprintf ppf "Type error: %s" s
  | `DivisionByZero -> Format.fprintf ppf "Division by zero"
  | `StepLimitReached -> Format.fprintf ppf "Step limit reached"
;;

type value =
  | VInt of int
  | VClosure of name * expr * env
  | VBuiltin of (value -> (value, error) Result.t)

and env = (name * value ref) list

type 'a result = ('a, error) Result.t

let return x = Result.Ok x

(* >>= (bind) - цепляет вычисления: если первое успешно, применяет функцию к результату *)
let ( >>= ) m f = Result.bind m ~f

(* >>| (map) - применяет функцию к успешному результату, не меняя контекст ошибки *)
let ( >>| ) m f = Result.map m ~f
let fail e = Result.Error e

let lookup env name =
  match List.Assoc.find env ~equal:String.equal name with
  | Some r -> return !r
  | None -> fail (`UnknownVariable name)
;;

let eval_binop op v1 v2 =
  match op, v1, v2 with
  | Add, VInt n1, VInt n2 -> return (VInt (n1 + n2))
  | Sub, VInt n1, VInt n2 -> return (VInt (n1 - n2))
  | Mul, VInt n1, VInt n2 -> return (VInt (n1 * n2))
  | Div, VInt _, VInt 0 -> fail `DivisionByZero
  | Div, VInt n1, VInt n2 -> return (VInt (n1 / n2))
  (* Операции сравнения - возвращают 1 (true) или 0 (false) *)
  | Eq, VInt n1, VInt n2 -> return (VInt (if n1 = n2 then 1 else 0))
  | Lt, VInt n1, VInt n2 -> return (VInt (if n1 < n2 then 1 else 0))
  | Gt, VInt n1, VInt n2 -> return (VInt (if n1 > n2 then 1 else 0))
  | Le, VInt n1, VInt n2 -> return (VInt (if n1 <= n2 then 1 else 0))
  | Ge, VInt n1, VInt n2 -> return (VInt (if n1 >= n2 then 1 else 0))
  (* Если типы не совпадают - ошибка типизации *)
  | _ -> fail (`TypeError "binary operation expects two integers")
;;

(** Основная функция вычисления выражений с подсчётом шагов *)
let rec eval_with_steps steps env expr =
  if !steps <= 0
  then fail `StepLimitReached
  else (
    Int.decr steps;
    match expr with
    | Const n -> return (VInt n)
    | Var x -> lookup env x
    | Abs (x, body) -> return (VClosure (x, body, env))
    | App (e1, e2) ->
      eval_with_steps steps env e1
      >>= (function
       | VClosure (x, body, closure_env) ->
         eval_with_steps steps env e2
         >>= fun v2 ->
         let new_env = (x, ref v2) :: closure_env in
         eval_with_steps steps new_env body
       | VBuiltin f -> eval_with_steps steps env e2 >>= fun v2 -> f v2
       | _ -> fail (`TypeError "application expects a function"))
    | BinOp (op, e1, e2) ->
      eval_with_steps steps env e1
      >>= fun v1 -> eval_with_steps steps env e2 >>= fun v2 -> eval_binop op v1 v2
    | If (cond, then_branch, else_branch) ->
      eval_with_steps steps env cond
      >>= (function
       | VInt 0 -> eval_with_steps steps env else_branch
       | VInt _ -> eval_with_steps steps env then_branch
       | _ -> fail (`TypeError "if condition must be an integer"))
    | Let (x, e1, e2) ->
      eval_with_steps steps env e1
      >>= fun v1 ->
      let new_env = (x, ref v1) :: env in
      eval_with_steps steps new_env e2
    | LetRec (f, x, body, in_expr) ->
      let dummy = VInt 0 in
      let rec_ref = ref dummy in
      let new_env = (f, rec_ref) :: env in
      let closure = VClosure (x, body, new_env) in
      rec_ref := closure;
      eval_with_steps steps new_env in_expr
    | Fix e ->
      eval_with_steps steps env e
      >>= (function
       | VClosure (x, body, closure_env) as closure ->
         let rec_ref = ref closure in
         let new_env = (x, rec_ref) :: closure_env in
         rec_ref := VClosure (x, body, new_env);
         eval_with_steps steps new_env body
       | _ -> fail (`TypeError "fix expects a closure"))
    | Prim ("println_int", [ e ]) ->
      eval_with_steps steps env e
      >>= fun v ->
      (match v with
       | VInt n ->
         Stdlib.print_endline (Int.to_string n);
         return v
       | VClosure _ | VBuiltin _ -> fail (`TypeError "println_int expects an integer"))
    | Prim (name, _) -> fail (`TypeError ("unknown primitive: " ^ name)))
;;

let builtin_println_int = function
  | VInt n ->
    Stdlib.print_endline (Int.to_string n);
    return (VInt n)
  | _ -> fail (`TypeError "println_int expects an integer")
;;

(* Начальное окружение со встроенными функциями *)
let initial_env = [ "println_int", ref (VBuiltin builtin_println_int) ]

(** Вычисление выражения с дефолтным лимитом шагов *)
let eval ?(step_limit = 100000) ?(env = initial_env) () expr =
  let steps = ref step_limit in
  eval_with_steps steps env expr
;;

let string_of_value = function
  | VInt n -> Int.to_string n
  | VClosure _ -> "<closure>"
  | VBuiltin _ -> "<builtin>"
;;

(** Парсинг и выполнение программы из строки *)
let parse_and_run ?(step_limit = 100000) str =
  match Parser.parse str with
  | Result.Error (`Parsing_error msg) -> fail (`TypeError ("Parse error: " ^ msg))
  | Result.Ok expr -> eval ~step_limit () expr >>| fun v -> string_of_value v
;;
