[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base

type value =
  | Unit
  | Num of int
  | Closure of varname * expr * env
  | RecClosure of varname * varname * expr * env

and env = (varname * value) list

type error =
  | UnboundVariable of varname
  | DivisionByZero
  | StepLimitExceeded
  | NonFunctionApplication of value
  | NonIntegerCondition of value
  | InvalidUnop of unop * value
  | InvalidBinop of binop * value * value
  | TypeError
  | LetWithoutBody
  | LetrecWithoutBody

module ResultM = struct
  type 'a t = ('a, error) result

  let return x = Ok x
  let fail e = Error e

  let ( >>= ) res f =
    match res with
    | Ok x -> f x
    | Error e -> Error e
  ;;

  let ( let* ) = ( >>= )
end

let ( let* ) = ResultM.( >>= )

open ResultM

let rec find_var x env =
  match env with
  | [] -> None
  | (y, v) :: rest -> if String.( = ) x y then Some v else find_var x rest
;;

let rec eval env steps (e : expr) =
  if steps <= 0
  then fail StepLimitExceeded
  else (
    match e with
    | Unit -> return (Unit, steps - 1)
    | Num n -> return (Num n, steps - 1)
    | Var x ->
      (match find_var x env with
       | Some v -> return (v, steps - 1)
       | None -> fail (UnboundVariable x))
    | Unop (op, e) ->
      let* v, st = eval env (steps - 1) e in
      (match v with
       | Num n ->
         let result =
           match op with
           | Inc -> Num (n + 1)
           | Dec -> Num (n - 1)
         in
         return (result, st)
       | _ -> fail (InvalidUnop (op, v)))
    | Binop (op, e1, e2) ->
      let* v1, st1 = eval env (steps - 1) e1 in
      let* v2, st2 = eval env st1 e2 in
      (match v1, v2 with
       | Num n1, Num n2 ->
         (match op with
          | Plus -> return (Num (n1 + n2), st2)
          | Minus -> return (Num (n1 - n2), st2)
          | Mult -> return (Num (n1 * n2), st2)
          | Div -> if n2 = 0 then fail DivisionByZero else return (Num (n1 / n2), st2))
       | _ -> fail (InvalidBinop (op, v1, v2)))
    | If (e1, e2, e3) ->
      let* cond, st1 = eval env (steps - 1) e1 in
      (match cond with
       | Num n -> if n > 0 then eval env st1 e2 else eval env st1 e3
       | _ -> fail (NonIntegerCondition cond))
    | Fun (x, body) -> return (Closure (x, body, env), steps - 1)
    | Let (x, e1, e2_opt) ->
      let* body, st = eval env (steps - 1) e1 in
      let new_env = (x, body) :: env in
      (match e2_opt with
       | Some exp -> eval new_env (st - 1) exp
       | None -> return (Unit, st - 1))
    | Letrec (x, e1, e2_opt) ->
      let* new_env =
        match e1 with
        | Fun (var, func) -> return ((x, RecClosure (x, var, func, env)) :: env)
        | _ -> fail TypeError
      in
      (match e2_opt with
       | Some e -> eval new_env (steps - 1) e
       | None -> return (Unit, steps - 1))
    | Fix e ->
      let* f, st = eval env (steps - 1) e in
      (match f with
       | Closure (arg, body, env') ->
         let fixed = RecClosure (arg, arg, body, env') in
         eval ((arg, fixed) :: env') st body
       | RecClosure (f_name, arg, body, env') ->
         let fixed = RecClosure (f_name, arg, body, env') in
         eval ((arg, fixed) :: (f_name, fixed) :: env') st body
       | _ -> fail (NonFunctionApplication f))
    | App (e1, e2) ->
      let* f, st1 = eval env (steps - 1) e1 in
      let* arg, st2 = eval env st1 e2 in
      (match f with
       | Closure (x, body, env') -> eval ((x, arg) :: env') (st2 - 1) body
       | RecClosure (f_name, x, body, env') ->
         let new_env = (x, arg) :: (f_name, RecClosure (f_name, x, body, env')) :: env' in
         eval new_env (st2 - 1) body
       | _ -> fail (NonFunctionApplication f))
    | Print e ->
      let* v, st = eval env (steps - 1) e in
      (match v with
       | Num n ->
         print_int n;
         print_newline ();
         return (v, st)
       | _ -> return (v, st)))
;;

let run_interpret program steps : (value, error) result =
  match eval [] steps program with
  | Ok (v, _) -> Ok v
  | Error e -> Error e
;;
