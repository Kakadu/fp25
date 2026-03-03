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
  | `UnknownVariable s -> Stdlib.Format.fprintf ppf "Unknown variable: %s" s
  | `TypeError s -> Stdlib.Format.fprintf ppf "Type error: %s" s
  | `DivisionByZero -> Stdlib.Format.fprintf ppf "Division by zero"
  | `StepLimitReached -> Stdlib.Format.fprintf ppf "Step limit reached"
;;

type value =
  | VInt of int
  | VClosure of name * expr * env
  | VRecClosure of name * name * expr * env
  | VBuiltin of (value -> (value, error) Result.t)

and env = (name * value) list

[@@@ocamlformat "disable"]

let return x = Result.Ok x

(* >>= (bind) *)
let ( >>= ) m f = Result.bind m ~f

(* >>| (map)*)
let ( >>| ) m f = Result.map m ~f
let fail e = Result.Error e

let lookup env name =
  match List.Assoc.find env ~equal:String.equal name with
  | Some v -> return v
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
  if steps <= 0
  then fail `StepLimitReached
  else
    let steps = steps - 1 in
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
         let new_env = (x, v2) :: closure_env in
         eval_with_steps steps new_env body
       | VRecClosure (fname, x, body, closure_env) ->
         eval_with_steps steps env e2
         >>= fun v2 ->
         let rec_closure = VRecClosure (fname, x, body, closure_env) in
         let new_env = (x, v2) :: (fname, rec_closure) :: closure_env in
         eval_with_steps steps new_env body
       | VBuiltin f -> eval_with_steps steps env e2 >>= f
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
      let new_env = (x, v1) :: env in
      eval_with_steps steps new_env e2
    | LetRec (f, x, body, in_expr) ->
      let rec_closure = VRecClosure (f, x, body, env) in
      let new_env = (f, rec_closure) :: env in
      eval_with_steps steps new_env in_expr
    | Fix e ->
      eval_with_steps steps env e
      >>= (function
       | VClosure (x, body, closure_env) ->
         let rec_closure = VRecClosure ("_fix", x, body, closure_env) in
         let new_env = (x, rec_closure) :: closure_env in
         eval_with_steps steps new_env body
       | _ -> fail (`TypeError "fix expects a closure"))
    | Prim ("println_int", [ e ]) ->
      eval_with_steps steps env e
      >>= fun v ->
      (match v with
       | VInt n ->
         Stdlib.print_endline (Int.to_string n);
         return v
       | VClosure _ -> fail (`TypeError "println_int expects an integer")
       | VRecClosure _ -> fail (`TypeError "println_int expects an integer")
       | VBuiltin _ -> fail (`TypeError "println_int expects an integer"))
    | Prim (name, _) -> fail (`TypeError ("unknown primitive: " ^ name))
;;

let builtin_println_int = function
  | VInt n ->
    Stdlib.print_endline (Int.to_string n);
    return (VInt n)
  | _ -> fail (`TypeError "println_int expects an integer")
;;

let initial_env = [ "println_int", VBuiltin builtin_println_int ]

let eval ?(step_limit = 100000) ?(env = initial_env) () expr =
  eval_with_steps step_limit env expr
;;

let string_of_value = function
  | VInt n -> Int.to_string n
  | VClosure _ -> "<closure>"  | VRecClosure _ -> "<rec-closure>"  | VBuiltin _ -> "<builtin>"
;;

(** Парсинг и выполнение программы из строки *)
let parse_and_run ?(step_limit = 100000) str =
  match Parser.parse str with
  | Result.Error (`Parsing_error msg) -> fail (`TypeError ("Parse error: " ^ msg))
  | Result.Ok expr -> eval ~step_limit () expr >>| string_of_value
