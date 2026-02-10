(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let rec compile_to_lambda_cbv : mlterm -> Lambda.lterm = function
  | App (Var "not", t) -> compile_to_lambda_cbv (ITE (t, Bool false, Bool true))
  | Var "fst" -> Abs ("p", App (Var "p", Lambda.ltrue))
  | Var "snd" -> Abs ("p", App (Var "p", Lambda.lfalse))
  | Var "inl" -> Abs ("x", Abs ("f", Abs ("g", App (Var "f", Var "x"))))
  | Var "inr" -> Abs ("x", Abs ("f", Abs ("g", App (Var "g", Var "x"))))
  | App (App (Var "&&", t1), t2) -> compile_to_lambda_cbv (ITE (t1, t2, Bool false))
  | App (App (Var "||", t1), t2) -> compile_to_lambda_cbv (ITE (t1, Bool true, t2))
  | Constr e -> Var e
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
  | LetExc (_, _, t) -> compile_to_lambda_cbv t
  | App (t1, t2) -> App (compile_to_lambda_cbv t1, compile_to_lambda_cbv t2)
  | Fun (x, t) -> Abs (x, compile_to_lambda_cbv t)
  | Pair (t1, t2) ->
    App
      ( App
          ( Abs ("x", Abs ("y", Abs ("f", App (App (Var "f", Var "x"), Var "y"))))
          , compile_to_lambda_cbv t1 )
      , compile_to_lambda_cbv t2 )
  | Match (t, v1, t1, v2, t2) ->
    let tl = compile_to_lambda_cbv t in
    let t1l = compile_to_lambda_cbv t1 in
    let t2l = compile_to_lambda_cbv t2 in
    App (App (tl, Abs (v1, t1l)), Abs (v2, t2l))
  | Try (t, l) ->
    Lambda.Try
      ( compile_to_lambda_cbv t
      , List.map (fun (s, v, t) -> s, Lambda.Abs (v, compile_to_lambda_cbv t)) l )
;;

let parse str =
  let lexer = Lexing.from_string str in
  try Parser.prog Lexer.token lexer with
  | Parser.Error ->
    let pos = lexer.Lexing.lex_curr_p in
    let line = pos.Lexing.pos_lnum in
    let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    let () = Printf.eprintf "Parser error at line %d, character %d\n" line char in
    raise (Failure "Parser error")
;;

let interp str =
  let ml_ast = parse str in
  let tp = Typing.hm_typechecker ml_ast in
  let l_ast = compile_to_lambda_cbv ml_ast in
  Lambda.beta_reduce Lambda.beta_step_in_cbv l_ast, tp
;;
