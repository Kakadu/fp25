[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Base.Result.Monad_infix

type error =
  | UnknownVariable of string
  | DivisionByZero
  | TypeMismatch
  | StepLimitExceeded

(** Values in our interpreter *)
type value =
  | VInt of int
  | VClosure of string * string Ast.t * environment
  | VBuiltin of string * (value -> (value, error) Result.t)
  | VUnit

and environment = (string * value) list

(** Lookup variable in environment *)
let lookup env name =
  match List.Assoc.find ~equal:String.equal env name with
  | Some v -> Ok v
  | None -> Error (UnknownVariable name)
;;

(** Evaluate binary operation *)
let eval_binop op v1 v2 =
  match op, v1, v2 with
  | Ast.Add, VInt a, VInt b -> Ok (VInt (a + b))
  | Ast.Sub, VInt a, VInt b -> Ok (VInt (a - b))
  | Ast.Mul, VInt a, VInt b -> Ok (VInt (a * b))
  | Ast.Div, VInt _, VInt 0 -> Error DivisionByZero
  | Ast.Div, VInt a, VInt b -> Ok (VInt (a / b))
  | Ast.Mod, VInt _, VInt 0 -> Error DivisionByZero
  | Ast.Mod, VInt a, VInt b -> Ok (VInt (a % b))
  | Ast.Eq, VInt a, VInt b -> Ok (VInt (if a = b then 1 else 0))
  | Ast.Neq, VInt a, VInt b -> Ok (VInt (if a <> b then 1 else 0))
  | Ast.Lt, VInt a, VInt b -> Ok (VInt (if a < b then 1 else 0))
  | Ast.Gt, VInt a, VInt b -> Ok (VInt (if a > b then 1 else 0))
  | Ast.Leq, VInt a, VInt b -> Ok (VInt (if a <= b then 1 else 0))
  | Ast.Geq, VInt a, VInt b -> Ok (VInt (if a >= b then 1 else 0))
  | _ -> Error TypeMismatch
;;

(** Основная функция вычисления со счётчиком шагов *)
let rec eval steps_remaining env expr =
  if steps_remaining <= 0
  then Error StepLimitExceeded
  else (
    let steps = steps_remaining - 1 in
    match expr with
    | Ast.Int n -> Ok (VInt n)
    | Ast.Var name -> lookup env name
    | Ast.Abs (param, body) -> Ok (VClosure (param, body, env))
    | Ast.BinOp (op, l, r) ->
      eval steps env l >>= fun vl -> eval steps env r >>= fun vr -> eval_binop op vl vr
    | Ast.If (cond, then_branch, else_branch_opt) ->
      eval steps env cond
      >>= (function
       | VInt 0 ->
         (match else_branch_opt with
          | Some else_branch -> eval steps env else_branch
          | None -> Ok (VInt 0))
       | VInt _ -> eval steps env then_branch
       | _ -> Error TypeMismatch)
    | Ast.Let (is_rec, name, binding, body) ->
      if is_rec
      then (
        match binding with
        | Ast.Abs (param, func_body) ->
          (* Рекурсивное окружение: функция ссылается сама на себя *)
          let rec new_env = (name, VClosure (param, func_body, new_env)) :: env in
          eval steps new_env body
        | _ ->
          eval steps env binding
          >>= fun value ->
          let new_env = (name, value) :: env in
          eval steps new_env body)
      else
        eval steps env binding
        >>= fun value ->
        let new_env = (name, value) :: env in
        eval steps new_env body
    | Ast.App (f, arg) ->
      eval steps env f
      >>= (function
       | VClosure (param, body, closure_env) ->
         eval steps env arg
         >>= fun varg ->
         let new_env = (param, varg) :: closure_env in
         eval steps new_env body
       | VBuiltin (_, builtin_fn) ->
         eval steps env arg >>= fun varg -> builtin_fn varg
       | _ -> Error TypeMismatch))
;;

(** Начальное окружение со встроенными функциями *)
let initial_env () =
  let print_builtin =
    VBuiltin
      ( "print"
      , fun v ->
          (match v with
           | VInt n -> Stdio.printf "%d\n%!" n
           | VClosure _ -> Stdio.printf "<fun>\n%!"
           | VBuiltin (name, _) -> Stdio.printf "<builtin:%s>\n%!" name
           | VUnit -> Stdio.printf "()\n%!");
          Ok VUnit )
  in
  [ "print", print_builtin ]
;;

(** Публичная функция для вычисления AST-выражения *)
let eval_expr ?(max_steps = 10000) ast = eval max_steps (initial_env ()) ast
;;
