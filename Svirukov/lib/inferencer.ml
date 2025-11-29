(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

type typ =
  | TInt
  | TFun of typ * typ
  | TUnit
  | TVar of string

type type_env = (string, typ, String.comparator_witness) Map.t

type type_error =
  | UnboundVar of string
  | TypeMismatch of typ * typ
  | OccursCheckError
  | InvalidCondition
  | ApplicationError
  | NotExpression

module TypeMonad = struct
  let return x = Ok x
  let fail msg = Error msg

  let ( >>= ) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;

  let ( let* ) = ( >>= )
end

let return = TypeMonad.return
let fail = TypeMonad.fail
let ( let* ) = TypeMonad.( let* )

let rec occurs_check var_name = function
  | TVar name when String.( = ) name var_name -> true
  | TFun (var, return_type) ->
    occurs_check var_name return_type || occurs_check var_name var
  | _ -> false
;;

let rec unify t1 t2 =
  match t1, t2 with
  | TInt, TInt -> return ()
  | TUnit, TUnit -> return ()
  | TVar name1, TVar name2 when String.( = ) name1 name2 -> return ()
  | TVar name, t | t, TVar name ->
    if occurs_check name t then fail OccursCheckError else return ()
  | TFun (arg1, ret1), TFun (arg2, ret2) ->
    let* () = unify arg1 arg2 in
    unify ret1 ret2
  | _ -> fail (TypeMismatch (t1, t2))
;;

type check_mode =
  | Expression
  | Statement

let typecheck_program expr =
  let rec check expr env counter mode =
    let fresh_var counter =
      let new_counter = counter + 1 in
      TVar (Int.to_string new_counter), new_counter
    in
    match expr with
    | Constant CUnit -> return (TUnit, counter)
    | Constant (CInt _) -> return (TInt, counter)
    | Var (PVar name) ->
      (match Map.find env name with
       | Some typ -> return (typ, counter)
       | None -> fail (UnboundVar name))
    | Binop (_, left, right) ->
      let* left_type, counter1 = check left env counter Expression in
      let* right_type, counter2 = check right env counter1 Expression in
      let* () = unify left_type TInt in
      let* () = unify right_type TInt in
      return (TInt, counter2)
    | Conditional (cond, then_expr, else_expr) ->
      let* cond_type, counter1 = check cond env counter Expression in
      let* () = unify cond_type TInt in
      let* then_type, counter2 = check then_expr env counter1 mode in
      (match else_expr with
       | Some else_expr ->
         let* else_type, counter3 = check else_expr env counter2 mode in
         let* () = unify then_type else_type in
         return (then_type, counter3)
       | None ->
         (match mode with
          | Expression ->
            let* () = unify then_type TUnit in
            return (TUnit, counter2)
          | Statement -> return (then_type, counter2)))
    | Func (PVar param, body) ->
      let param_type, counter1 = fresh_var counter in
      let new_env = Map.set env ~key:param ~data:param_type in
      let* body_type, counter2 = check body new_env counter1 Expression in
      return (TFun (param_type, body_type), counter2)
    | App (func, arg) ->
      let* func_type, counter1 = check func env counter Expression in
      let* arg_type, counter2 = check arg env counter1 Expression in
      let return_type, counter3 = fresh_var counter2 in
      let* () = unify func_type (TFun (arg_type, return_type)) in
      return (return_type, counter3)
    | Let (NonRec, PVar name, value_expr, body_opt) ->
      let* value_type, counter1 = check value_expr env counter Expression in
      let new_env = Map.set env ~key:name ~data:value_type in
      (match body_opt with
       | Some body -> check body new_env counter1 mode
       | None ->
         (match mode with
          | Expression -> fail NotExpression
          | Statement -> return (TUnit, counter1)))
    | Let (Rec, PVar name, value_expr, body_opt) ->
      let func_type, counter1 = fresh_var counter in
      let temp_env = Map.set env ~key:name ~data:func_type in
      let* actual_type, counter2 = check value_expr temp_env counter1 Expression in
      if occurs_check (Int.to_string counter2) actual_type
      then fail OccursCheckError
      else
        let* () = unify func_type actual_type in
        let final_env = Map.set env ~key:name ~data:actual_type in
        (match body_opt with
         | Some body -> check body final_env counter2 mode
         | None ->
           (match mode with
            | Expression -> fail NotExpression
            | Statement -> return (TUnit, counter2)))
  in
  let empty_env = Map.empty (module String) in
  match check expr empty_env 0 Statement with
  | Ok (typ, _) -> Ok typ
  | Error e -> Error e
;;
