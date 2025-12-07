[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Ast

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end

(** Real monadic interpreter goes here *)
type error = [ `UnknownVariable of string (** just for example *) ]

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type RESULTMONAD = sig
  include MONAD

  val fail : string -> 'a t
end

module Res : RESULTMONAD with type 'a t = ('a, string) Result.t = struct
  type 'a t = ('a, string) Result.t

  let return x = Ok x
  let fail failmsg = Error failmsg

  let ( >>= ) a b =
    match a with
    | Ok x -> b x
    | Error x -> Error x
  ;;
end

let ( let* ) = Res.( >>= )

(* runtime values *)
type envvalue =
  | EVal of int (* evaluated some expression in just an int *)
  | EClosure of ident * Ast.expr * envt
    (* closure of non rec function -> name of arg for call, function code,
       function environment available *)
  | ERecClosure of ident * ident * Ast.expr * envt
(* closure of rec function -> name of function
   to add to environment to call, arg name, code, function environment *)

and envt = (ident * envvalue) list

let init : envt = []

let rec lookup (env : envt) (name : ident) =
  match env with
  | [] -> Res.fail "no bound var"
  | (var, code) :: _ when String.equal var name -> Res.return code
  | _ :: tl -> lookup tl name
;;

let rec eval env steps expression =
  if steps = 0
  then Res.fail "programm freezed"
  else (
    match expression with
    | Ast.Const x -> Res.return (EVal x)
    | Ast.Ident x ->
      let* value = lookup env x in
      Res.return value
    | Ast.Binexpr (op, l, r) ->
      let* left = eval env (steps - 1) l in
      let* right = eval env (steps - 1) r in
      (match left, right with
       | EVal lv, EVal rv ->
         (match op with
          | Ast.Plus -> Res.return (EVal (lv + rv))
          | Ast.Minus -> Res.return (EVal (lv - rv))
          | Ast.Mul -> Res.return (EVal (lv * rv))
          | Ast.Div ->
            (match rv with
             | 0 -> Res.fail "div by zero"
             | _ -> Res.return (EVal (lv / rv)))
          | Ast.Eq -> Res.return (if lv = rv then EVal 1 else EVal 0)
          | Ast.Neq -> Res.return (if lv != rv then EVal 1 else EVal 0)
          | Ast.Bi -> Res.return (if lv > rv then EVal 1 else EVal 0)
          | Ast.Le -> Res.return (if lv < rv then EVal 1 else EVal 0)
          | _ -> Res.fail "not implemented")
       | _ -> Res.fail "bad binop")
    | Ast.Ite (cond, tb, eb) ->
      let* condition = eval env (steps - 1) cond in
      (match condition with
       | EVal x when x = 0 -> eval env (steps - 1) eb
       | EVal _ -> eval env (steps - 1) tb
       | _ -> Res.fail "bad condition")
    | Ast.Let (b, name, letexpr, inexpr) ->
      (match b with
       | Ast.Recflag false ->
         (match name with
          | Ast.Ident f ->
            let* code = eval env (steps - 1) letexpr in
            eval ((f, code) :: env) (steps - 2) inexpr
          | _ -> Res.fail "bad function name")
       | Ast.Recflag true ->
         (match name with
          | Ast.Ident f ->
            (match letexpr with
             | Ast.Abs (Ast.Ident arg, e) ->
               eval ((f, ERecClosure (f, arg, e, env)) :: env) (steps - 1) inexpr
             | _ -> Res.fail "not a function")
          | _ -> Res.fail "bad function name"))
    | Ast.Abs (Ast.Ident arg, f) -> Res.return (EClosure (arg, f, env))
    | Ast.App (f, arg) ->
      let* func = eval env (steps - 1) f in
      let* evarg = eval env (steps - 1) arg in
      (match func with
       | EClosure (argname, fcode, fenv) ->
         eval ((argname, evarg) :: fenv) (steps - 1) fcode
       | ERecClosure (fname, argname, fcode, fenv) ->
         eval
           ((argname, evarg) :: (fname, ERecClosure (fname, argname, fcode, fenv)) :: fenv)
           (steps - 1)
           fcode
       | _ -> Res.fail "wtf")
    | _ -> Res.fail "")
;;
