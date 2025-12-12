[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base

type error =
  | UnboundVariable of string
  | DivisionByZero
  | TypeError of string
  | UnsupportedConstruct of string
  | IncorrectExpression
  | StackOverflow

type 'a eval_result = ('a, error) Result.t

module Env = Stdlib.Map.Make (String)

let ( >>= ) r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e
;;

let ( let* ) = ( >>= )

type value =
  | VInt of int
  | VUnit
  | VClosure of string * expr * env ref
  | VBuiltin of (value -> value eval_result)

and env = value Env.t

let return x = Ok x
let err e = Error e

let eval_binop op v1 v2 =
  match op, v1, v2 with
  | Plus, VInt a, VInt b -> return (VInt (a + b))
  | Minus, VInt a, VInt b -> return (VInt (a - b))
  | Mult, VInt a, VInt b -> return (VInt (a * b))
  | Div, VInt _, VInt 0 -> err DivisionByZero
  | Div, VInt a, VInt b -> return (VInt (a / b))
  | Equal, VInt a, VInt b -> return (VInt (if a = b then 1 else 0))
  | More, VInt a, VInt b -> return (VInt (if a > b then 1 else 0))
  | Less, VInt a, VInt b -> return (VInt (if a < b then 1 else 0))
  | EMore, VInt a, VInt b -> return (VInt (if a >= b then 1 else 0))
  | ELess, VInt a, VInt b -> return (VInt (if a <= b then 1 else 0))
  | _, _, _ -> err (TypeError "binary operation expected two integers")
;;

let rec eval (env : env) (e : expr) (steps : int) : value eval_result =
  if steps < 0
  then err StackOverflow
  else (
    let steps = steps - 1 in
    match e with
    | Int n -> return (VInt n)
    | Var x ->
      (match Env.find_opt x env with
       | Some v -> return v
       | None -> err (UnboundVariable x))
    | BinOp (op, e1, e2) ->
      eval env e1 steps >>= fun v1 -> eval env e2 steps >>= fun v2 -> eval_binop op v1 v2
    | If (condition, then_expr, else_expr_opt) ->
      eval env condition steps
      >>= fun rcond ->
      (match rcond with
       | VInt 0 ->
         (match else_expr_opt with
          | Some else_expr -> eval env else_expr steps
          | None -> return VUnit)
       | VInt _ -> eval env then_expr steps
       | _ -> err IncorrectExpression)
    | Let (flag, name, bound_expr, body_opt) ->
      (match flag with
       | NonRec ->
         eval env bound_expr steps
         >>= fun v ->
         let new_env = Env.add name v env in
         (match body_opt with
          | Some body -> eval new_env body steps
          | None -> return VUnit)
       | Rec ->
         let env_ref = ref env in
         (* Создаем замыкание, которое ссылается на этот env_ref *)
         let* v =
           eval !env_ref bound_expr steps
           >>= function
           | VClosure (param, body, _) ->
             (* Создаем новое замыкание с правильной ссылкой *)
             return (VClosure (param, body, env_ref))
           | other -> return other
         in
         (* Обновляем окружение, добавляя туда само замыкание *)
         let new_env = Env.add name v !env_ref in
         env_ref := new_env;
         (* Теперь вычисляем тело в обновленном окружении *)
         (match body_opt with
          | Some body -> eval new_env body steps
          | None -> return VUnit))
    | Abs (param_expr, body) ->
      (match param_expr with
       | Var x ->
         (* Создаем замыкание с ссылкой на текущее окружение *)
         let env_ref = ref env in
         return (VClosure (x, body, env_ref))
       | _ -> err (UnsupportedConstruct "lambda parameter must be a variable"))
    | App (fun_expr, arg_expr) ->
      let* vf = eval env fun_expr steps in
      let* va = eval env arg_expr steps in
      (match vf with
       | VClosure (param, body, closure_env_ref) ->
         (* Добавляем параметр в окружение замыкания *)
         let new_env = Env.add param va !closure_env_ref in
         eval new_env body steps
       | VBuiltin f -> f va
       | _ -> err (TypeError "application expects a function")))
;;

let init_evn () =
  let print_int_fun =
    VBuiltin
      (function
        | VInt x ->
          let () = print_int x in
          let () = print_newline () in
          return VUnit
        | _ -> err (TypeError "print_int expects an integer"))
  in
  Env.empty |> Env.add "print_int" print_int_fun
;;

let run_interpret expr = eval (init_evn ()) expr 200

let string_of_value = function
  | VInt n -> string_of_int n
  | _ -> "()"
;;

let string_of_error = function
  | UnboundVariable name -> Printf.sprintf "Unbound variable: %s" name
  | DivisionByZero -> "Division by zero"
  | TypeError msg -> Printf.sprintf "Type error: %s" msg
  | UnsupportedConstruct constr -> Printf.sprintf "Unsupported construct: %s" constr
  | IncorrectExpression -> "Incorrect expression"
  | StackOverflow -> "Stack overflow"
;;
