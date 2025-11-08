(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Utils

type counter = int
type result = string Ast.t * counter

(* Smart constructors *)
let var x = EVar x
let abs x y = EAbs (x, y)
let app x y = EApp (x, y)

(* no longer needed? *)
(* rework this file to accept brujin lambdas *)
let replace_name x ~by =
  let rec helper = function
    | EVar y when String.equal x y -> EVar by
    | EVar t -> EVar t
    | EApp (l, r) -> EApp (helper l, helper r)
    | EAbs (y, t) when String.equal x y -> EAbs (by, helper t)
    | EAbs (z, t) -> EAbs (z, helper t)
  in
  helper
;;

let rec next_name s old =
  if List.mem ~equal:String.equal old s then next_name ("_" ^ s) old else s
;;

(*  The call [subst x ~by:v e] means `[x/v]e` or `e[v -> x]` *)
let subst x ~by:v =
  let rec helper = function
    | EVar y when String.equal y x -> v
    | EVar y -> EVar y
    | EApp (l, r) -> app (helper l) (helper r)
    | EAbs (y, b) when String.equal y x -> abs y b
    | EAbs (y, t) when is_free_in y v ->
      let equal = String.equal in
      let frees = free_vars ~equal v @ free_vars ~equal t in
      let w = next_name y frees in
      helper (abs w (replace_name y ~by:w t))
    | EAbs (y, b) -> abs y (helper b)
  in
  helper
;;

type strat =
  { on_var : strat -> counter -> name -> result
  ; on_abs : strat -> counter -> name -> string Ast.t -> result
  ; on_app : strat -> counter -> string Ast.t -> string Ast.t -> result
  }

let without_strat =
  let on_var _ c n = var n, c - 1 in
  let on_abs _ c n m = abs n m, c - 1 in
  let on_app _ c n m = app n m, c - 1 in
  { on_var; on_abs; on_app }
;;

let apply_strat st count ast =
  let st = if count = 0 then without_strat else st in
  match ast with
  | EVar name -> st.on_var st count name
  | EAbs (x, b) -> st.on_abs st count x b
  | EApp (l, r) -> st.on_app st count l r
;;

let cbn_strat =
  let on_app st cnt f arg =
    match apply_strat st cnt f with
    | EAbs (x, e), cnt -> apply_strat st cnt (subst x ~by:arg e)
    | f2, cnt -> app f2 arg, cnt
  in
  { without_strat with on_app }
;;

let under_abstraction st cnt x b =
  let b, c = apply_strat st cnt b in
  abs x b, c
;;

(* Normal Order Reduction to Normal Form
   Application function reduced as CBN first
   + Reduce under abstractions *)
let nor_strat =
  let on_app st cnt f arg =
    match apply_strat cbn_strat cnt f with
    | EAbs (x, e), cnt -> apply_strat st cnt @@ subst x ~by:arg e
    | f1, cnt ->
      let f2, cnt = apply_strat st cnt f1 in
      let arg2, cnt = apply_strat st cnt arg in
      app f2 arg2, cnt
  in
  { without_strat with on_app; on_abs = under_abstraction }
;;

(* Call-by-Value Reduction to Weak Normal Form *)
let cbv_strat =
  let on_app st cnt f arg =
    match apply_strat st cnt f with
    | EAbs (x, e), cnt ->
      let arg2, cnt = apply_strat st cnt arg in
      apply_strat st cnt @@ subst x ~by:arg2 e
    | f2, cnt ->
      let b, cnt = apply_strat st cnt arg in
      app f2 b, cnt
  in
  { without_strat with on_app }
;;

(* Applicative Order Reduction to Normal Form
   As CBV but reduce under abstractions *)
let ao_strat = { cbv_strat with on_abs = under_abstraction }
let a = var "a"
let x = var "x"
let y = var "y"
let z = var "z"
let f = var "f"
let g = var "g"
let h = var "h"
let m = var "m"
let n = var "n"
let p = var "p"
let zero = abs "f" @@ abs "x" x
let one = abs "f" @@ abs "x" @@ app f x
let two = abs "f" @@ abs "x" @@ app f (app f x)
let three = abs "f" @@ abs "x" @@ app f (app f (app f x))
