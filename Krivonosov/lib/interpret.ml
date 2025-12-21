[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)

open Base

(** Monad signature with fail *)
module type MonadFail = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end

type error =
  [ `UnknownVariable of string
  | `DivisionByZero
  | `TypeMismatch
  | `StepLimitExceeded
  ]

(** Values in our interpreter *)
type value =
  | VInt of int
  | VClosure of string * string Ast.t * environment
  | VBuiltin of string * (value -> (value, error) Base.Result.t)
  | VUnit

and environment = (string * value) list

module Interpret (M : MonadFail) : sig
  val run : max_steps:int -> string Ast.t -> (value, error) M.t
end = struct
  open M

  (** Lookup variable in environment *)
  let lookup env name =
    match List.Assoc.find ~equal:String.equal env name with
    | Some v -> return v
    | None -> fail (`UnknownVariable name)
  ;;

  (** Evaluate binary operation *)
  let eval_binop op v1 v2 =
    match op, v1, v2 with
    | Ast.Add, VInt a, VInt b -> return (VInt (a + b))
    | Ast.Sub, VInt a, VInt b -> return (VInt (a - b))
    | Ast.Mul, VInt a, VInt b -> return (VInt (a * b))
    | Ast.Div, VInt _, VInt 0 -> fail `DivisionByZero
    | Ast.Div, VInt a, VInt b -> return (VInt (a / b))
    | Ast.Mod, VInt _, VInt 0 -> fail `DivisionByZero
    | Ast.Mod, VInt a, VInt b -> return (VInt (a % b))
    | Ast.Eq, VInt a, VInt b -> return (VInt (if a = b then 1 else 0))
    | Ast.Neq, VInt a, VInt b -> return (VInt (if a <> b then 1 else 0))
    | Ast.Lt, VInt a, VInt b -> return (VInt (if a < b then 1 else 0))
    | Ast.Gt, VInt a, VInt b -> return (VInt (if a > b then 1 else 0))
    | Ast.Leq, VInt a, VInt b -> return (VInt (if a <= b then 1 else 0))
    | Ast.Geq, VInt a, VInt b -> return (VInt (if a >= b then 1 else 0))
    | _ -> fail `TypeMismatch
  ;;

  (** Main evaluation function with step counter as parameter *)
  let rec eval steps_remaining env expr =
    if steps_remaining <= 0
    then fail `StepLimitExceeded
    else (
      let steps = steps_remaining - 1 in
      match expr with
      | Ast.Int n -> return (VInt n)
      | Ast.Var name -> lookup env name
      | Ast.Abs (param, body) -> return (VClosure (param, body, env))
      | Ast.BinOp (op, l, r) ->
        eval steps env l >>= fun vl -> eval steps env r >>= fun vr -> eval_binop op vl vr
      | Ast.If (cond, then_branch, else_branch_opt) ->
        eval steps env cond
        >>= (function
         | VInt 0 ->
           (match else_branch_opt with
            | Some else_branch -> eval steps env else_branch
            | None -> return (VInt 0))
         | VInt _ -> eval steps env then_branch
         | _ -> fail `TypeMismatch)
      | Ast.Let (is_rec, name, binding, body) ->
        if is_rec
        then (
          (* For recursive let, create environment with self-reference *)
          match binding with
          | Ast.Abs (param, func_body) ->
            (* Create closure with recursive environment *)
            let rec new_env = (name, VClosure (param, func_body, new_env)) :: env in
            eval steps new_env body
          | _ ->
            (* Non-function recursive bindings not supported yet *)
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
           eval steps env arg
           >>= fun varg ->
           (match builtin_fn varg with
            | Base.Result.Ok v -> return v
            | Base.Result.Error e -> fail e)
         | _ -> fail `TypeMismatch))
  ;;

  (** Create initial environment with built-in functions *)
  let initial_env () =
    (* Print function: prints a value and returns unit *)
    let print_builtin =
      VBuiltin
        ( "print"
        , fun v ->
            (match v with
             | VInt n -> Stdio.printf "%d\n%!" n
             | VClosure _ -> Stdio.printf "<fun>\n%!"
             | VBuiltin (name, _) -> Stdio.printf "<builtin:%s>\n%!" name
             | VUnit -> Stdio.printf "()\n%!");
            Base.Result.Ok VUnit )
    in
    [ "print", print_builtin ]
  ;;

  let run ~max_steps expr = eval max_steps (initial_env ()) expr
end

(** Public function to evaluate an AST expression *)
let eval_expr ?(max_steps = 10000) ast =
  let module I = Interpret (Base.Result) in
  I.run ~max_steps ast
;;
