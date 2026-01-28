(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let rec compile_to_lambda_cbv : mlterm -> Lambda.lterm = function
  | App (Var "not", t) -> compile_to_lambda_cbv (ITE (t, Bool false, Bool true))
  | App (App (Var "&&", t1), t2) -> compile_to_lambda_cbv (ITE (t1, t2, Bool false))
  | App (App (Var "||", t1), t2) -> compile_to_lambda_cbv (ITE (t1, Bool true, t2))
  | Var x -> Var x
  | Int i -> Int i
  | Bool true -> Lambda.ltrue
  | Bool false -> Lambda.lfalse
  | Unit -> Unit
  | ITE (c, th, e) ->
    let cl = compile_to_lambda_cbv c in
    let thl = compile_to_lambda_cbv th in
    let el = compile_to_lambda_cbv e in
    let u = Lambda.fresh_var (Lambda.vars cl @ Lambda.vars thl @ Lambda.vars el) in
    App (App (App (cl, Abs (u, thl)), Abs (u, el)), Unit)
  | Let (v, t1, t2) -> App (Abs (v, compile_to_lambda_cbv t2), compile_to_lambda_cbv t1)
  | LetRec (v, t1, t2) ->
    App
      ( Abs (v, compile_to_lambda_cbv t2)
      , App (Lambda.fix_comb_cbv, Abs (v, compile_to_lambda_cbv t1)) )
  | App (t1, t2) -> App (compile_to_lambda_cbv t1, compile_to_lambda_cbv t2)
  | Fun (x, t) -> Abs (x, compile_to_lambda_cbv t)
;;

let parse str =
  let lexer = Lexing.from_string str in
  Parser.prog Lexer.token lexer
;;

let interp str =
  let ml_ast = parse str in
  let tp, _, _ = Typing.hm_typechecker ml_ast in
  let l_ast = compile_to_lambda_cbv ml_ast in
  Lambda.beta_reduce Lambda.beta_step_in_cbv l_ast, tp
;;
