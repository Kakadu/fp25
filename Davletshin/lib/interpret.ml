[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)
open Ast

type error =
  | UnknownVariable of string
  | TypeError of string
  | DivisionByZero

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  module Syntax : sig
    (* A synonym for >>= *)
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type MONADERROR = sig
  include MONAD

  val fail : error -> 'a t
end

module RESULT : MONADERROR with type 'a t = ('a, error) Result.t = struct
  type 'a t = ('a, error) Result.t

  let return x = Ok x
  let fail x = Error x

  let ( let* ) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;

  module Syntax = struct
    let ( let* ) m f =
      match m with
      | Ok x -> f x
      | Error e -> Error e
    ;;
  end
end

module Interpret (M : MONADERROR) : sig
  val run : name Ast.t -> int M.t
end = struct
  module Env = Map.Make (String)

  type env = value Env.t

  and value =
    | VInt of int
    | VClosure of string * string Ast.t * env

  let vint n = M.return (VInt n)
  let vclosure n e env = M.return (VClosure (n, e, env))

  (** [eval env e] is the [<env, e> ==> v] relation. *)
  let rec eval env e =
    match e with
    | Int n -> vint n
    | Var x -> eval_var env x
    | Abs (x, b) -> vclosure x b env
    | App (l, r) -> eval_app env l r
    | Binop (op, l, r) -> eval_binop env op l r
    | Unop (op, e) -> eval_unop env op e
    | If (c, t, e) -> eval_if env c t e
    | Let (Nonrec, n, e1, e2) -> eval_let env n e1 e2
    | Let (Rec, n, e1, e2) -> eval_let env n e1 e2

  (** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
  and eval_var env x =
    try M.return (Env.find x env) with
    | Not_found -> M.fail (UnknownVariable x)

  (** [eval_app env e1 e2] is the [v] such that [<env, e1 e2> ==> v]. *)
  and eval_app env e1 e2 =
    let open M.Syntax in
    let* v1 = eval env e1 in
    match v1 with
    | VClosure (n, body, defenv) ->
      let* v2 = eval env e2 in
      eval (Env.add n v2 defenv) body
    | _ -> M.fail (TypeError "Tried to apply non-function")

  and eval_binop env op e1 e2 =
    let open M.Syntax in
    let* v1 = eval env e1 in
    let* v2 = eval env e2 in
    match op, v1, v2 with
    | Plus, VInt a, VInt b -> vint (a + b)
    | Minus, VInt a, VInt b -> vint (a - b)
    | Times, VInt a, VInt b -> vint (a * b)
    | Divide, VInt _, VInt 0 -> M.fail DivisionByZero
    | Divide, VInt a, VInt b -> vint (a / b)
    | Eq, VInt a, VInt b -> vint (if a = b then 1 else 0)
    | Neq, VInt a, VInt b -> vint (if a <> b then 1 else 0)
    | Lt, VInt a, VInt b -> vint (if a < b then 1 else 0)
    | Gt, VInt a, VInt b -> vint (if a > b then 1 else 0)
    | Le, VInt a, VInt b -> vint (if a <= b then 1 else 0)
    | Ge, VInt a, VInt b -> vint (if a >= b then 1 else 0)
    | _ -> M.fail (TypeError "Invalid binary operation")

  and eval_unop env op e =
    let open M.Syntax in
    let* v = eval env e in
    match op, v with
    | Pos, VInt a -> vint (+a)
    | Neg, VInt a -> vint (-a)
    | _ -> M.fail (TypeError "Invalid unary operation")

  and eval_if env c t e =
    let open M.Syntax in
    let* guard = eval env c in
    match guard with
    | VInt 0 -> eval env e
    | VInt _ -> eval env t
    | _ -> M.fail (TypeError "if guard must be an integer")

  and eval_let env n e1 e2 =
    let open M.Syntax in
    let* v1 = eval env e1 in
    eval (Env.add n v1 env) e2

  and print (e : value) =
    match e with
    | VInt n -> M.return n
    | VClosure _ -> M.fail (TypeError "Tried to return non-integer")
  ;;

  let run e =
    let open M.Syntax in
    let* v = eval Env.empty e in
    print v
  ;;
end

let parse_and_run str =
  let module I = Interpret (RESULT) in
  match Parser.parse str with
  | Ok ast ->
    (match I.run ast with
     | Ok n -> Printf.printf "Success: %d\n" n
     | Error err ->
       (match err with
        | UnknownVariable x -> Format.eprintf "UnknownVariable: %s\n" x
        | TypeError msg -> Format.eprintf "TypeError: %s\n" msg
        | DivisionByZero -> Format.eprintf "Division by zero\n"))
  | Error _ ->
    Format.eprintf "Parsing error\n%!";
    exit 1
;;
