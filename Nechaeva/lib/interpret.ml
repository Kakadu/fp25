(** Copyright 2021-2025, Kakadu and contributors *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** Типы - сначала error, потом value, чтобы избежать циклических зависимостей *)
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
  | VRecClosure of string * string list * expr * (string * value) list
  | VBuiltin of (value -> (value, error) result)

type env = (string * value) list

(** Встроенная функция print *)
let builtin_print v =
  match v with
  | VInt n -> 
    print_int n; 
    print_newline (); 
    Ok VUnit
  | _ -> Error (TypeError "print expects integer")

(** Монада - как в слайдах Simple_state, но с ошибками *)
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
  
  let return x = fun steps -> (steps, Ok x)
  
  let ( let* ) m f = fun steps0 ->
    let steps1, res = m steps0 in
    match res with
    | Ok x -> f x steps1
    | Error e -> (steps1, Error e)
  
  let fail e = fun steps -> (steps, Error e)
  
  let get_steps = fun steps -> (steps, Ok steps)
  
  let dec_steps = fun steps ->
    if steps <= 0 then (steps, Error ExceedMaxSteps)
    else (steps - 1, Ok ())
  
  let run m init = m init
end

(** Работа с окружением - без Base *)
let env_empty = []

let env_extend env x v = (x, v) :: env

let env_lookup env name =
  try
    Ok (List.assoc name env)
  with Not_found ->
    Error (UnboundVariable name)

(** Начальное окружение *)
let initial_env =
  let env = env_extend env_empty "print" (VBuiltin builtin_print) in
  let env = env_extend env "true" (VInt 1) in
  env_extend env "false" (VInt 0)

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
        apply func arg_val env
    | Let (NonRecursive, name, e1, e2) ->
        let* v1 = eval e1 env in
        let new_env = env_extend env name v1 in
        eval e2 new_env
    | Let (Recursive, name, e1, e2) ->
    (match e1 with
     | Abs (params, body) ->
         (* Создаём рекурсивное замыкание *)
         let rec_closure = VRecClosure (name, params, body, env) in
         let new_env = env_extend env name rec_closure in
         eval e2 new_env
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
              | Div -> 
                  if b = 0 then fail DivisionByZero
                  else return (VInt (a / b)))
         | _ -> fail (TypeError "Binary operation expects integers"))
    | Comp (op, e1, e2) ->
        let* v1 = eval e1 env in
        let* v2 = eval e2 env in
        (match v1, v2 with
         | VInt a, VInt b ->
             let result = match op with
               | Equal -> a = b
               | NotEqual -> a <> b
               | Less -> a < b
               | LessEq -> a <= b
               | Greater -> a > b
               | GreaterEq -> a >= b
             in return (VInt (if result then 1 else 0))
         | _ -> fail (TypeError "Comparison expects integers"))
    | If (cond, e1, e2) ->
        let* cond_val = eval cond env in
        match cond_val with
        | VInt 0 -> eval e2 env
        | VInt _ -> eval e1 env
        | _ -> fail (TypeError "If condition must be integer")
  
  and apply func arg_val env =
    match func with
    | VClosure ([], body, closure_env) ->
        eval body closure_env
    | VClosure (p :: ps, body, closure_env) ->
        let new_env = env_extend closure_env p arg_val in
        if ps = [] 
        then eval body new_env
        else return (VClosure (ps, body, new_env))
    | VRecClosure (f_name, params, body, closure_env) ->
        let env_with_self = env_extend closure_env f_name func in
        apply (VClosure (params, body, env_with_self)) arg_val env
    | VBuiltin f ->
        (match f arg_val with
         | Ok v -> return v
         | Error e -> fail e)
    | _ -> fail NotAFunction
end
let run_interpreter expr max_steps =
  match StepErrorM.run (Interpreter.eval expr initial_env) max_steps with
  | _, Ok value -> Ok value
  | _, Error e -> Error e
