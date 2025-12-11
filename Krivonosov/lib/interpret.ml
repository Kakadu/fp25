[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)

open Base
open Utils

type error =
  [ `UnknownVariable of string
  | `DivisionByZero
  | `TypeMismatch
  ]

(** Values in our interpreter *)
type value =
  | VInt of int
  | VClosure of string * string Ast.t * environment

and environment = (string * value) list

module Interpret (M : MONAD_FAIL) : sig
  val run : string Ast.t -> (value, [> error ]) M.t
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
    | Ast.Mod, VInt a, VInt b -> return (VInt (a mod b))
    | Ast.Eq, VInt a, VInt b -> return (VInt (if a = b then 1 else 0))
    | Ast.Neq, VInt a, VInt b -> return (VInt (if a <> b then 1 else 0))
    | Ast.Lt, VInt a, VInt b -> return (VInt (if a < b then 1 else 0))
    | Ast.Gt, VInt a, VInt b -> return (VInt (if a > b then 1 else 0))
    | Ast.Leq, VInt a, VInt b -> return (VInt (if a <= b then 1 else 0))
    | Ast.Geq, VInt a, VInt b -> return (VInt (if a >= b then 1 else 0))
    | _ -> fail `TypeMismatch
  ;;

  (** Main evaluation function *)
  let rec eval env = function
    | Ast.Int n -> return (VInt n)
    | Ast.Var name -> lookup env name
    | Ast.Abs (param, body) -> return (VClosure (param, body, env))
    | Ast.BinOp (op, l, r) ->
      eval env l >>= fun vl -> eval env r >>= fun vr -> eval_binop op vl vr
    | Ast.If (cond, then_branch, else_branch_opt) ->
      eval env cond
      >>= (function
       | VInt 0 ->
         (match else_branch_opt with
          | Some else_branch -> eval env else_branch
          | None -> return (VInt 0))
       | VInt _ -> eval env then_branch
       | _ -> fail `TypeMismatch)
    | Ast.Let (is_rec, name, binding, body) ->
      if is_rec
      then (
        (* For recursive let, create environment with self-reference *)
        match binding with
        | Ast.Abs (param, func_body) ->
          (* Create closure with recursive environment *)
          let rec new_env = (name, VClosure (param, func_body, new_env)) :: env in
          eval new_env body
        | _ ->
          (* Non-function recursive bindings not supported yet *)
          eval env binding
          >>= fun value ->
          let new_env = (name, value) :: env in
          eval new_env body)
      else
        eval env binding
        >>= fun value ->
        let new_env = (name, value) :: env in
        eval new_env body
    | Ast.App (f, arg) ->
      eval env f
      >>= (function
       | VClosure (param, body, closure_env) ->
         eval env arg
         >>= fun varg ->
         let new_env = (param, varg) :: closure_env in
         eval new_env body
       | _ -> fail `TypeMismatch)
  ;;

  let run expr = eval [] expr
end

let parse_and_run str =
  let module I = Interpret (Base.Result) in
  let rez = Base.Result.(Parser.parse str >>= I.run) in
  match rez with
  | Result.Ok (VInt n) -> Stdlib.Printf.printf "Success: %d\n" n
  | Result.Ok (VClosure _) -> Stdlib.Printf.printf "Success: <closure>\n"
  | Result.Error #Parser.error ->
    Format.eprintf "Parsing error\n%!";
    Stdlib.exit 1
  | Result.Error (`UnknownVariable name) ->
    Format.eprintf "Interpreter error: Unknown variable %s\n%!" name;
    Stdlib.exit 1
  | Result.Error `DivisionByZero ->
    Format.eprintf "Interpreter error: Division by zero\n%!";
    Stdlib.exit 1
  | Result.Error `TypeMismatch ->
    Format.eprintf "Interpreter error: Type mismatch\n%!";
    Stdlib.exit 1
;;
