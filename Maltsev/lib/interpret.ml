[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Ast
open Stdlib

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
  | EUnit (* unit return type like *)
  | Ebuiltin of ident (* type for built in functions which are added in init *)

and envt = (ident * envvalue) list

let init : envt = [ "print", Ebuiltin "print" ]

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
          | Ast.Le -> Res.return (if lv < rv then EVal 1 else EVal 0))
       | _ -> Res.fail "bad binop")
    | Ast.Ite (cond, tb, eb) ->
      let* condition = eval env (steps - 1) cond in
      (match condition with
       | EVal x when x = 0 -> eval env (steps - 1) eb
       | EVal _ -> eval env (steps - 1) tb
       | _ -> Res.fail "bad condition")
    | Ast.Let (b, name, letexpr, inexpr) ->
      if b
      then (
        match letexpr with
        | Ast.Abs (arg, e) ->
          let rec recclos = (name, EClosure (arg, e, recclos)) :: env in
          eval recclos (steps - 1) inexpr
        | _ -> Res.fail "not a function")
      else
        let* code = eval env (steps - 1) letexpr in
        eval ((name, code) :: env) (steps - 2) inexpr
    | Ast.Abs (arg, f) -> Res.return (EClosure (arg, f, env))
    | Ast.App someapp ->
      let* func =
        match someapp with
        | Var (s, _) -> lookup env s
        | Fun (x, e, _) -> eval env (steps - 1) (Ast.Abs (x, e))
        | Application (a, _) -> eval env (steps - 1) (Ast.App a)
      in
      let* evarg =
        match someapp with
        | Var (_, e) -> eval env (steps - 1) e
        | Fun (_, _, e) -> eval env (steps - 1) e
        | Application (_, e) -> eval env (steps - 1) e
      in
      (match func with
       | EClosure (argname, fcode, fenv) ->
         eval ((argname, evarg) :: fenv) (steps - 1) fcode
       | Ebuiltin s when s = "print" ->
         (match evarg with
          | EVal x ->
            Printf.printf "%d\n" x;
            Res.return EUnit
          | _ -> Res.fail "can print ints only")
       | _ -> Res.fail "can apply functions only"))
;;
