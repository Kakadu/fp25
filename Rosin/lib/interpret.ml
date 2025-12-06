[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type value =
  | VUnit
  | VNum of int
  | VClosure of varname * expr * env
  | VRecClosure of varname * varname * expr * env

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

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module ResultM : MONAD with type 'a t = ('a, error) Result.t = struct
  type 'a t = ('a, error) Result.t

  let return x = Ok x
  let fail msg = Error msg

  let ( let* ) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;
end

open ResultM

let rec find_var x = function
  | [] -> None
  | (y, v) :: _ when String.equal x y -> Some v
  | (_, _) :: rest -> find_var x rest
;;

let rec eval env steps e =
  if steps <= 0
  then fail StepLimitExceeded
  else (
    match e with
    | Num n -> return (VNum n, steps - 1)
    | Var x ->
      (match find_var x env with
       | Some v -> return (v, steps - 1)
       | None -> fail (UnboundVariable x))
    | Unop (op, e) ->
      let* v, st = eval env (steps - 1) e in
      (match v with
       | VNum n ->
         let result =
           match op with
           | Inc -> VNum (n + 1)
           | Dec -> VNum (n - 1)
         in
         return (result, st)
       | _ -> fail (InvalidUnop (op, v)))
    | Binop (op, e1, e2) ->
      let* v1, st1 = eval env (steps - 1) e1 in
      let* v2, st2 = eval env (st1 - 1) e2 in
      (match v1, v2 with
       | VNum n1, VNum n2 ->
         (match op with
          | Plus -> return (VNum (n1 + n2), st2 - 1)
          | Minus -> return (VNum (n1 - n2), st2 - 1)
          | Mult -> return (VNum (n1 * n2), st2 - 1)
          | Div -> if n2 = 0 then fail DivisionByZero else return (VNum (n1 / n2), st2 - 1))
       | _ -> fail (InvalidBinop (op, v1, v2)))
    | If (e1, e2, e3_opt) ->
      let* cond, st1 = eval env (steps - 1) e1 in
      (match cond with
       | VNum n when n > 0 -> eval env (st1 - 1) e2
       | VNum _ ->
         (match e3_opt with
          | Some e3 -> eval env (st1 - 1) e3
          | None -> return (VUnit, st1 - 1))
       | _ -> fail (NonIntegerCondition cond))
    | Fun (x, body) -> return (VClosure (x, body, env), steps - 1)
    | Let (x, e1, e2) ->
      let* body, st = eval env (steps - 1) e1 in
      let new_env = (x, body) :: env in
      eval new_env (st - 1) e2
    | Letrec (x, e1, e2) ->
      let* new_env =
        match e1 with
        | Fun (var, func) -> return ((x, VRecClosure (x, var, func, env)) :: env)
        | _ -> fail TypeError
      in
      eval new_env (steps - 1) e2
    | Fix e ->
      let* f, st = eval env (steps - 1) e in
      (match f with
       | VClosure (f_param, body, closure_env) ->
         (match body with
          | Fun (n_param, inner_body) ->
            let rec_closure = VRecClosure (f_param, n_param, inner_body, closure_env) in
            return (rec_closure, st - 1)
          | _ ->
            let rec_closure = VRecClosure (f_param, f_param, body, closure_env) in
            return (rec_closure, st - 1))
       | VRecClosure (f_name, param, body, closure_env) ->
         return (VRecClosure (f_name, param, body, closure_env), st - 1)
       | _ -> fail (NonFunctionApplication f))
    | App (e1, e2) ->
      let* f, st1 = eval env (steps - 1) e1 in
      let* arg, st2 = eval env st1 e2 in
      (match f with
       | VClosure (x, body, env') -> eval ((x, arg) :: env') (st2 - 1) body
       | VRecClosure (f_name, x, body, env') ->
         let new_env = (x, arg) :: (f_name, f) :: env' in
         eval new_env (st2 - 1) body
       | _ -> fail (NonFunctionApplication f))
    | Print e ->
      let* v, st = eval env (steps - 1) e in
      (match v with
       | VNum n ->
         Stdio.printf "%d\n" n;
         return (v, st)
       | _ -> return (v, st)))
;;

let run_interpret program steps : (value, error) result =
  match eval [] steps program with
  | Ok (v, _) -> Ok v
  | Error e -> Error e
;;
