(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

type value =
  | VInt of int
  | VClosure of pattern * expr

type env = (string, value, Base.String.comparator_witness) Base.Map.t

type error =
  | UnboundVariable of string
  | TypeError of string
  | DivisionByZero
  | ParttialApplication
  | TooManyArgs
  | ExceedNumberOfSteps of expr
  | Unimplemented

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

let ( let* ) = ResultM.( let* )

module Env : sig
  val init : env
  val add_val : env -> string -> value -> env
  val ok_or_novar : env -> string -> value ResultM.t
end = struct
  let init = Map.empty (module String)
  let add_val env key value = Map.set env ~key ~data:value

  let ok_or_novar env name =
    match Map.find env name with
    | Some v -> ResultM.return v
    | None -> ResultM.fail (UnboundVariable name)
  ;;
end

let rec substitute expr varname value =
  match expr with
  | Constant _ -> expr
  | Var (PVar name) when Base.String.( = ) name varname -> value
  | Var _ -> expr
  | Binop (op, left, right) ->
    Binop (op, substitute left varname value, substitute right varname value)
  | Conditional (cond, then_expr, else_expr) ->
    Conditional
      ( substitute cond varname value
      , substitute then_expr varname value
      , match else_expr with
        | Some e -> Some (substitute e varname value)
        | None -> None )
  | Let (rec_flag, pattern, value_expr, body_expr) ->
    (match pattern with
     | PVar bound_name when Base.String.( = ) bound_name varname -> expr
     | _ ->
       Let
         ( rec_flag
         , pattern
         , substitute value_expr varname value
         , match body_expr with
           | Some e -> Some (substitute e varname value)
           | None -> None ))
  | Func (PVar param, _) when Base.String.( = ) param varname -> expr
  | Func (param, body) -> Func (param, substitute body varname value)
  | App (func, arg) -> App (substitute func varname value, substitute arg varname value)
;;

let rec eval exp env step =
  if step = 0
  then ResultM.fail (ExceedNumberOfSteps exp)
  else (
    match exp with
    | Constant CUnit -> ResultM.return (Constant CUnit)
    | Constant (CInt n) -> ResultM.return (Constant (CInt n))
    | Var (PVar name) ->
      let* binding = Env.ok_or_novar env name in
      (match binding with
       | VInt n -> ResultM.return (Constant (CInt n))
       | VClosure (pat, body) -> ResultM.return (Func (pat, body)))
    | Binop (op, left, right) ->
      let eval_binop expr =
        match eval expr env (step - 1) with
        | Ok (Constant (CInt n)) -> ResultM.return n
        | Ok _ -> ResultM.fail (TypeError "Can do binop only with const int")
        | Error er -> ResultM.fail er
      in
      let* l = eval_binop left in
      let* r = eval_binop right in
      (match op with
       | Plus -> ResultM.return (Constant (CInt (l + r)))
       | Minus -> ResultM.return (Constant (CInt (l - r)))
       | Asteriks -> ResultM.return (Constant (CInt (l * r)))
       | Dash when r = 0 -> ResultM.fail DivisionByZero
       | Dash -> ResultM.return (Constant (CInt (l / r)))
       | Equals -> ResultM.return (Constant (CInt (if l = r then 1 else 0)))
       | MoreThan -> ResultM.return (Constant (CInt (if l > r then 1 else 0)))
       | LessThan -> ResultM.return (Constant (CInt (if l < r then 1 else 0)))
       | EqLess -> ResultM.return (Constant (CInt (if l <= r then 1 else 0)))
       | EqMore -> ResultM.return (Constant (CInt (if l >= r then 1 else 0))))
    | Let (NonRec, PVar name, body, cont) ->
      let* body = eval body env (step - 1) in
      let* letval =
        match body with
        | Constant (CInt n) -> ResultM.return (VInt n)
        | Func (pat, inner) -> ResultM.return (VClosure (pat, inner))
        | _ -> ResultM.fail (TypeError "can put only vars and funcs in env")
      in
      let new_env = Env.add_val env name letval in
      (match cont with
       | Some exp -> eval exp new_env (step - 1)
       | None -> ResultM.return (Constant CUnit))
    | Let (Rec, PVar name, body, cont) ->
      let* new_env =
        match body with
        | Func (PVar var, func) ->
          ResultM.return (Env.add_val env name (VClosure (PVar var, func)))
        | _ -> ResultM.fail (TypeError "can put only vars and funcs in env")
      in
      (match cont with
       | Some e -> eval e new_env (step - 1)
       | None -> ResultM.return (Constant CUnit))
    | Conditional (cond, ifbr, elsebr) ->
      let* cond = eval cond env (step - 1) in
      (match cond with
       | Constant (CInt n) when n = 1 -> eval ifbr env (step - 1)
       | Constant (CInt n) when n = 0 ->
         (match elsebr with
          | Some e -> eval e env (step - 1)
          | None -> ResultM.return (Constant CUnit))
       | _ -> ResultM.fail (TypeError "not a number in cond evaluation"))
    | App (_, _) ->
      let rec application core env args =
        match args, core with
        | [], expr -> eval expr env (step - 1)
        | arg :: tail, Func (PVar name, body) ->
          let new_body = substitute body name arg in
          application new_body env tail
        | _ :: _, _ -> ResultM.fail TooManyArgs
      in
      let rec form_args expr list =
        match expr with
        | App (left, arg) ->
          let* arg = eval arg env (step - 1) in
          form_args left (List.append [ arg ] list)
        | _ -> ResultM.return (list, expr)
      in
      let* args, body = form_args exp [] in
      let* new_body =
        match body with
        | Var (PVar name) ->
          let* binding = eval body env (step - 1) in
          ResultM.return (substitute body name binding)
        | Func (_, _) -> ResultM.return body
        | _ -> ResultM.fail (TypeError "can only apply args to funcs")
      in
      application new_body env args
    | Func (pat, ex) -> ResultM.return (Func (pat, ex)))
;;

let run_interpret expr steps =
  match eval expr Env.init steps with
  | Ok exp -> Ok exp
  | Error er -> Error er
;;
