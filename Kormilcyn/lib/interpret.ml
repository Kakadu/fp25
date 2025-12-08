[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)

open Utils
open Ast

type error = [ `NotAValue of string ]

module Interpret (M : MONAD_FAIL) : sig
  val run : _ Ast.t -> (int, [> error ]) M.t
end = struct
  (* TODO: negation *)
  (* TODO: recursion *)
  (* TODO: exception handling *)

  type 'name value =
    | VInt of int
    | VClosure of 'name * 'name Ast.t * 'name env
    | VExn

  and 'name env = ('name * 'name value) list

  let rec eval (env : 'name env) (e : 'name Ast.t) : 'name value =
    match e with
    | Int i -> VInt i
    | Var x -> resolve env x
    | Bin (bop, e1, e2) -> eval_bin env eval bop e1 e2
    | Let (x, e1, e2) -> eval_let env x e1 e2
    | If (e1, e2, e3) -> eval_if env e1 e2 e3
    | Fun (x, e) -> eval_fun env x e
    | App (e1, e2) -> eval_app env e1 e2
    | Exn -> VExn

  and resolve env x =
    match env with
    | [] -> VExn
    | h :: _ when fst h = x -> snd h
    | _ :: t -> resolve t x

  and eval_bin env eval bop e1 e2 =
    match bop, eval env e1, eval env e2 with
    | Add, VInt a, VInt b -> VInt (a + b)
    | Sub, VInt a, VInt b -> VInt (a - b)
    | Mul, VInt a, VInt b -> VInt (a * b)
    | Div, VInt a, VInt b -> VInt (a / b)
    | Leq, VInt a, VInt b -> VInt (if a <= b then 1 else 0)
    | _ -> VExn

  and eval_let env x e1 e2 =
    let v1 = eval env e1 in
    let env' = (x, v1) :: env in
    eval env' e2

  and eval_if env e1 e2 e3 =
    match eval env e1 with
    | VInt 1 -> eval env e2
    | VInt 0 -> eval env e3
    | _ -> VExn

  and eval_fun env x e = VClosure (x, e, env)

  and eval_app env e1 e2 =
    match eval env e1 with
    | VClosure (x, e, defenv) ->
      let v2 = eval env e2 in
      let defenv' = (x, v2) :: defenv in
      eval defenv' e
    | _ -> VExn
  ;;

  let int_of_value = function
    | VInt i -> M.return i
    | VClosure _ -> M.fail (`NotAValue "cannot represent function as integer")
    | VExn -> M.fail (`NotAValue "interpretation exception")
  ;;

  let run (e : 'a Ast.t) : (int, [> error ]) M.t = int_of_value (eval [] e)
end

let parse_and_run str =
  let module I = Interpret (Base.Result) in
  let rez = Base.Result.(Parser.parse str >>= I.run) in
  match rez with
  | Result.Ok n -> Printf.printf "Success: %d\n" n
  | Result.Error #Parser.error ->
    Format.eprintf "Parsing error\n%!";
    exit 1
  | Result.Error #error ->
    Format.eprintf "Interpreter error\n%!";
    exit 1
;;
