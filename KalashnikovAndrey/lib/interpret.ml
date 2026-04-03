[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Parser
open Printer

type error = 
  | Division_by_zero 
  | Var_unbound of string
  | Out_of_steps
  | Type_error of string 
let pp_error ppf = function
  | Division_by_zero ->
      Format.fprintf ppf "Division by zero"
  | Var_unbound s -> 
      Format.fprintf ppf "Unbound variable '%s'" s
  | Out_of_steps ->
      Format.fprintf ppf "Out of steps"
  | Type_error s -> 
      Format.fprintf ppf "Type error: %s" s

module StateError = struct
  type 'a t = int -> ('a, error) result * int
  let return x steps = (Ok x, steps)
  let bind m f steps =
    let (res, steps') = m steps in
    match res with
    | Ok x -> f x steps'          
    | Error e -> (Error e, steps') 
  let step steps = if steps <= 0 then (Error Out_of_steps, steps) else 
    (Ok (), steps - 1)
  let fail err steps = (Error err, steps)
end

type value = 
  | VInt of int
  | VClosure of string * expr * env 
  | VBuiltin of (value -> value StateError.t)
and env = (string * value) list

let ( let* ) = StateError.bind

let return x = Ok x
let fail e = Error e 

let builtin_print = function
  | VInt n ->
      Format.printf "%d\n%!" n;
      StateError.return (VInt n)
  | _ -> StateError.fail (Type_error "print expects an integer")

let initial_env = [ "print", VBuiltin builtin_print ]

let rec check_env key env  : value StateError.t = 
  match env with
  | [] -> StateError.fail (Var_unbound key) 
  | (k, v) :: rest -> if k = key then StateError.return v else check_env key rest

let rec eval (env : env) (e : expr) : value StateError.t= 
  match e with
  | Const n -> 
    let* _ = StateError.step  in  StateError.return (VInt n)
  | Var x -> check_env x env
  
  | Neg expr -> 
      let* _ = StateError.step in
      let* v = eval env expr in
      (match v with 
       | VInt i -> StateError.return (VInt (-i)) 
       | _ -> StateError.fail (Type_error "Negation expects an integer"))

  | BinOp (op, l, r) ->
      let* _ = StateError.step  in 
      let* v1 = eval env l in
      let* v2 = eval env r in
      (match v1, v2 with
      | VInt i1, VInt i2 ->
          (match op with
          | Add -> StateError.return (VInt (i1 + i2))
          | Sub -> StateError.return (VInt (i1 - i2))
          | Mul -> StateError.return (VInt (i1 * i2))
          | Div ->
              if i2 = 0 then StateError.fail Division_by_zero
              else StateError.return (VInt (i1 / i2))
          | Lt -> StateError.return (VInt (if i1 < i2 then 1 else 0))
          | Eq -> StateError.return (VInt (if i1 = i2 then 1 else 0))
          | Mt -> StateError.return (VInt (if i1 > i2 then 1 else 0)))
      | _ -> StateError.fail (Type_error "Binary operations require integers"))

  | If (cond, t, e) ->
      let* _ = StateError.step  in
      let* v_cond = eval env cond in
      (match v_cond with
       | VInt i -> if i <> 0 then eval env t else eval env e
       | _ -> StateError.fail (Type_error "If condition must be an integer"))

  | Fun (arg, body) -> 
      let* _ = StateError.step  in
      StateError.return (VClosure (arg, body, env))

  | App (func_expr, arg_expr) ->
      let* _ = StateError.step  in
      let* func_val = eval env func_expr in
      let* arg_val  = eval env arg_expr in
      (match func_val with
       | VClosure (param, body, closure_env) ->
           let new_env = (param, arg_val) :: closure_env in
           eval new_env body
       | VBuiltin f ->
           f arg_val
       | _ -> StateError.fail (Type_error "Application of non-function"))

  | Let (rec_flag, name, expr, body) ->
      let* _ = StateError.step  in
      (match rec_flag with
      | Val -> 
          let* v = eval env expr in
          eval ((name, v) :: env) body
      | Rec ->
          match expr with
          | Fun (arg, fn_body) ->
              let rec new_env = (name, VClosure(arg, fn_body, new_env)) :: env in
              eval new_env body
          | _ -> StateError.fail (Type_error "Let rec expects a function"))

let pp_value ppf = function
  | VInt n -> Format.fprintf ppf "%d" n
  | VClosure _ -> Format.fprintf ppf "<closure>"
  | VBuiltin _ -> Format.fprintf ppf "<builtin>"

let run_eval input step printer_flag =
  match parse input with
  | Error err ->
      Format.printf "Parse error: %a\n%!" Parser.pp_error err
  | Ok ast -> 
      match eval initial_env ast step with
        | Ok value, steps when printer_flag ->
          Format.printf "Ast: %a\n%!" pp ast;
          Format.printf "Value: %a\nSteps: %d\n%!" pp_value value steps
        | Ok value, steps ->
          Format.printf "Value: %a\nSteps: %d\n%!" pp_value value steps
        | Error err, steps ->
          Format.printf "Error: %a\nSteps: %d\n%!" pp_error err steps
