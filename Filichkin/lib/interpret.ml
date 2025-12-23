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
  | StepCountIsZero

type 'a eval_result = ('a, error) Result.t

let ( let* ) r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e
;;

type env = (string * value) list

and value =
  | VInt of int
  | VUnit
  | VClosure of string * expr * env
  | VBuiltin of (value -> value eval_result)

let rec look_up x = function
  | [] -> None
  | (y, v) :: _ when String.equal x y -> Some v
  | _ :: tl -> look_up x tl
;;

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

(** Main interpretation function *)
let rec eval (env : env) (e : expr) (steps : int) : value eval_result =
  if steps < 0
  then err StepCountIsZero
  else (
    let steps = steps - 1 in
    match e with
    | Int n -> return (VInt n)
    | Var x ->
      (match look_up x env with
       | Some v -> return v
       | None -> err (UnboundVariable x))
    | BinOp (op, e1, e2) ->
      let* v1 = eval env e1 steps in
      let* v2 = eval env e2 steps in
      eval_binop op v1 v2
    | If (cond, then_e, else_opt) ->
      let* v = eval env cond steps in
      (match v with
       | VInt 0 ->
         (match else_opt with
          | Some e -> eval env e steps
          | None -> return VUnit)
       | VInt _ -> eval env then_e steps
       | _ -> err IncorrectExpression)
    | Let (flag, name, bound, body_opt) ->
      (match flag with
       | NonRec ->
         let* v = eval env bound steps in
         let new_env = (name, v) :: env in
         (match body_opt with
          | Some body -> eval new_env body steps
          | None -> return VUnit)
       | Rec ->
         (match bound with
          | Abs (param, fun_body) ->
            let rec closure = VClosure (param, fun_body, rec_env)
            and rec_env = (name, closure) :: env in
            (match body_opt with
             | Some body -> eval rec_env body steps
             | None -> return VUnit)
          | _ -> err (TypeError "recursive binding must be a function")))
    | Abs (param, body) -> return (VClosure (param, body, env))
    | UnOp (op, e1) ->
      let* v1 = eval env e1 steps in
      (match op, v1 with
       | "-", VInt n -> return (VInt (-n))
       | _ -> err (TypeError "unsupported unary operator"))
    | App (f, arg) ->
      let* vf = eval env f steps in
      let* va = eval env arg steps in
      (match vf with
       | VClosure (param, body, closure_env) ->
         let new_env = (param, va) :: closure_env in
         eval new_env body steps
       | VBuiltin f -> f va
       | _ -> err (TypeError "application expects a function")))
;;

let init_env =
  let print_int_fun =
    VBuiltin
      (function
        | VInt x ->
          Stdlib.Printf.printf "%d\n" x;
          return VUnit
        | _ -> err (TypeError "print_int expects an integer"))
  in
  [ "print_int", print_int_fun ]
;;

let run_interpret expr = eval init_env expr 1000

let string_of_value = function
  | VInt n -> Int.to_string n
  | _ -> "()"
;;

let string_of_error = function
  | UnboundVariable name -> Printf.sprintf "Unbound variable: %s" name
  | DivisionByZero -> "Division by zero"
  | TypeError msg -> Printf.sprintf "Type error: %s" msg
  | UnsupportedConstruct constr -> Printf.sprintf "Unsupported construct: %s" constr
  | IncorrectExpression -> "Incorrect expression"
  | StepCountIsZero -> "Step count is zero"
;;
