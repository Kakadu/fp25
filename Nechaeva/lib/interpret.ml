[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type error =
  | UnboundVariable of string
  | TypeError of string
  | DivisionByZero
  | NotAFunction
  | ExceedMaxSteps

type value =
  | VInt of int
  | VUnit
  | VClosure of string list * expr * (string * value) list
  | VBuiltin of (value -> (value, error) result)

type env = (string * value) list

let builtin_print = function
  | VInt n ->
    print_int n;
    print_newline ();
    Ok VUnit
  | _ -> Error (TypeError "print expects integer")
;;

module StepErrorM : sig
  type 'a t = int -> int * ('a, error) result

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : error -> 'a t
  val get_steps : int t
  val dec_steps : unit t
  val run : 'a t -> int -> int * ('a, error) result
end = struct
  type 'a t = int -> int * ('a, error) result

  let return x steps = steps, Ok x

  let ( let* ) m f steps0 =
    let steps1, res = m steps0 in
    match res with
    | Ok x -> f x steps1
    | Error e -> steps1, Error e
  ;;

  let fail e steps = steps, Error e
  let get_steps steps = steps, Ok steps

  let dec_steps steps =
    if steps <= 0 then steps, Error ExceedMaxSteps else steps - 1, Ok ()
  ;;

  let run m = m
end

let env_empty = []
let env_extend env x v = (x, v) :: env

let env_lookup env name =
  try Ok (List.assoc name env) with
  | Not_found -> Error (UnboundVariable name)
;;

let initial_env =
  let env = env_extend env_empty "print" (VBuiltin builtin_print) in
  let env = env_extend env "true" (VInt 1) in
  env_extend env "false" (VInt 0)
;;

module Interpreter = struct
  open StepErrorM

  let rec eval (expr : expr) (env : env) : value t =
    let* () = dec_steps in
    match expr with
    | Const (Int n) -> return (VInt n)
    | Const Unit -> return VUnit
    | Var name ->
      (match env_lookup env name with
       | Ok v -> return v
       | Error e -> fail e)
    | Abs (params, body) -> return (VClosure (params, body, env))
    | App (f, arg) ->
      let* func = eval f env in
      let* arg_val = eval arg env in
      apply func arg_val
    | Let (NonRecursive, name, e1, e2) ->
      let* v1 = eval e1 env in
      let new_env = env_extend env name v1 in
      eval e2 new_env
    | Let (Recursive, name, e1, e2) ->
      (match e1 with
       | Abs (params, body) ->
         let rec closure = VClosure (params, body, rec_env)
         and rec_env = (name, closure) :: env in
         eval e2 rec_env
       | _ -> fail (TypeError "let rec must bind to function"))
    | BinOp (op, e1, e2) ->
      let* v1 = eval e1 env in
      let* v2 = eval e2 env in
      (match v1, v2 with
       | VInt a, VInt b ->
         (match op with
          | Plus -> return (VInt (a + b))
          | Minus -> return (VInt (a - b))
          | Mul -> return (VInt (a * b))
          | Div -> if b = 0 then fail DivisionByZero else return (VInt (a / b)))
       | _ -> fail (TypeError "Binary operation expects integers"))
    | UnOp (Neg, e) ->
      let* v = eval e env in
      (match v with
       | VInt n -> return (VInt (-n))
       | _ -> fail (TypeError "Unary minus expects integer"))
    | Comp (op, e1, e2) ->
      let* v1 = eval e1 env in
      let* v2 = eval e2 env in
      (match v1, v2 with
       | VInt a, VInt b ->
         let result =
           match op with
           | Equal -> a = b
           | NotEqual -> a <> b
           | Less -> a < b
           | LessEq -> a <= b
           | Greater -> a > b
           | GreaterEq -> a >= b
         in
         return (VInt (if result then 1 else 0))
       | _ -> fail (TypeError "Comparison expects integers"))
    | If (cond, e1, e2) ->
      let* cond_val = eval cond env in
      (match cond_val with
       | VInt 0 -> eval e2 env
       | VInt _ -> eval e1 env
       | _ -> fail (TypeError "If condition must be integer"))

  and apply func arg_val =
    match func with
    | VClosure ([], body, closure_env) -> eval body closure_env
    | VClosure (p :: ps, body, closure_env) ->
      let new_env = env_extend closure_env p arg_val in
      (match ps with
       | [] -> eval body new_env
       | _ -> return (VClosure (ps, body, new_env)))
    | VBuiltin f ->
      (match f arg_val with
       | Ok v -> return v
       | Error e -> fail e)
    | _ -> fail NotAFunction
  ;;
end

let run_interpreter expr max_steps =
  match StepErrorM.run (Interpreter.eval expr initial_env) max_steps with
  | _, Ok value -> Ok value
  | _, Error e -> Error e
;;
