(** Copyright 2026, Kirill K. Smirnov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Lambda

let rec mlterm_to_string : mlterm -> string = function
  | Var x -> x
  | Int i -> Printf.sprintf "%d" i
  | Bool true -> "true"
  | Bool false -> "false"
  | Unit -> "()"
  | ITE (c, th, e) ->
    "(if "
    ^ mlterm_to_string c
    ^ " then "
    ^ mlterm_to_string th
    ^ " else "
    ^ mlterm_to_string e
    ^ ")"
  | Let (v, t1, t2) ->
    "let " ^ v ^ "=" ^ mlterm_to_string t1 ^ " in " ^ mlterm_to_string t2
  | LetRec (v, t1, t2) ->
    "let rec " ^ v ^ "=" ^ mlterm_to_string t1 ^ " in " ^ mlterm_to_string t2
  | App (t1, t2) -> "(" ^ mlterm_to_string t1 ^ " " ^ mlterm_to_string t2 ^ ")"
  | Fun (x, t2) -> "fun " ^ x ^ " -> " ^ mlterm_to_string t2
;;

let rec compile_to_lambda_cbv : mlterm -> lterm = function
  | Var x -> Var x
  | Int i -> Int i
  | Bool true -> ltrue
  | Bool false -> lfalse
  | Unit -> Unit
  | ITE (c, th, e) ->
    let cl = compile_to_lambda_cbv c in
    let thl = compile_to_lambda_cbv th in
    let el = compile_to_lambda_cbv e in
    let u = fresh_var (vars cl @ vars thl @ vars el) in
    App (App (App (cl, Abs (u, thl)), Abs (u, el)), Unit)
  | Let (v, t1, t2) -> App (Abs (v, compile_to_lambda_cbv t2), compile_to_lambda_cbv t1)
  | LetRec (v, t1, t2) ->
    App
      ( Abs (v, compile_to_lambda_cbv t2)
      , App (fix_comb_cbv, Abs (v, compile_to_lambda_cbv t1)) )
  | App (t1, t2) -> App (compile_to_lambda_cbv t1, compile_to_lambda_cbv t2)
  | Fun (x, t) -> Abs (x, compile_to_lambda_cbv t)
;;

let interp str =
  let lexer = Lexing.from_string str in
  let ml_ast = Parser.prog Lexer.token lexer in
  let l_ast = compile_to_lambda_cbv ml_ast in
  beta_reduce beta_step_in_cbv l_ast
;;
