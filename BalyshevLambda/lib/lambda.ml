(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Utils

(* Smart constructors *)
let var x = Var x
let abs x y = Abs (x, y)
let app x y = App (x, y)

let replace_name x ~by =
  let rec helper = function
    | Var y when String.equal x y -> Var by
    | Var t -> Var t
    | App (l, r) -> App (helper l, helper r)
    | Abs (y, t) when String.equal x y -> Abs (by, helper t)
    | Abs (z, t) -> Abs (z, helper t)
  in
  helper
;;

let rec next_name s old =
  if List.mem ~equal:String.equal old s then next_name ("_" ^ s) old else s
;;

(*  The call [subst x ~by:v e] means `[x/v]e` or `e[v -> x]` *)
let subst x ~by:v =
  let rec helper = function
    | Var y when String.equal y x -> v
    | Var y -> Var y
    | App (l, r) -> app (helper l) (helper r)
    | Abs (y, b) when String.equal y x -> abs y b
    | Abs (y, t) when is_free_in y v ->
      let frees = free_vars v @ free_vars t in
      let w = next_name y frees in
      helper (abs w (replace_name y ~by:w t))
    | Abs (y, b) -> abs y (helper b)
  in
  helper
;;

type strat =
  { on_var : strat -> name -> string Ast.t
  ; on_abs : strat -> name -> string Ast.t -> string Ast.t
  ; on_app : strat -> string Ast.t -> string Ast.t -> string Ast.t
  }

let apply_strat st = function
  | Var name -> st.on_var st name
  | Abs (x, b) -> st.on_abs st x b
  | App (l, r) -> st.on_app st l r
;;

let without_strat =
  let on_var _ = var in
  let on_abs _ = abs in
  let on_app _ = app in
  { on_var; on_abs; on_app }
;;

let cbn_strat =
  let on_app st f arg =
    match apply_strat st f with
    | Abs (x, e) -> apply_strat st (subst x ~by:arg e)
    | f2 -> App (f2, arg)
  in
  { without_strat with on_app }
;;

let under_abstraction st x b = abs x (apply_strat st b)

(* Normal Order Reduction to Normal Form
   Application function reduced as CBN first
   + Reduce under abstractions *)
let nor_strat =
  let on_app st f arg =
    match apply_strat cbn_strat f with
    | Abs (x, e) -> apply_strat st @@ subst x ~by:arg e
    | f1 ->
      let f2 = apply_strat st f1 in
      let arg2 = apply_strat st arg in
      App (f2, arg2)
  in
  { without_strat with on_app; on_abs = under_abstraction }
;;

(* Call-by-Value Reduction to Weak Normal Form *)
let cbv_strat =
  let on_app st f arg =
    match apply_strat st f with
    | Abs (x, e) ->
      let arg2 = apply_strat st arg in
      apply_strat st @@ subst x ~by:arg2 e
    | f2 -> App (f2, apply_strat st arg)
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

open Ast
open Utils

type 'a status =
  | Done of 'a
  | WIP of 'a

let fin x = Done x
let wip x = WIP x

let ao_small_step_strat =
  let rec helper = function
    | Var _ as l -> fin l
    | Abs (x, b) ->
      (match helper b with
       | WIP b2 -> wip (abs x b2)
       | Done b2 -> fin (abs x b2))
    | App (f, arg) ->
      (match helper f with
       | WIP f2 -> wip (app f2 arg)
       | Done (Abs (x, body)) ->
         (match helper arg with
          | Done arg -> wip (subst x ~by:arg body)
          | WIP arg -> wip (app f arg))
       | Done f2 -> fin (App (f2, arg)))
  in
  let rec loop t =
    match helper t with
    | Done x -> x
    | WIP x ->
      Format.printf " -- %a\n%!" Pprintast.pp_hum x;
      loop x
  in
  let on_app _ f arg = loop (app f arg) in
  let on_abs _ f x = loop (abs f x) in
  let on_var _ x = loop (var x) in
  { on_var; on_abs; on_app }
;;

type 'a limited_t =
  | Over of 'a
  | NotOver of 'a * int

type limited_expr = Ast.name Ast.t limited_t

type limited_strat =
  { on_var : limited_strat -> Ast.name limited_t -> limited_expr
  ; on_abs : limited_strat -> (Ast.name * string Ast.t) limited_t -> limited_expr
  ; on_app : limited_strat -> (string Ast.t * string Ast.t) limited_t -> limited_expr
  }

let over x = Over x
let not_over x lim = NotOver (x, lim)

let apply_limited_strat strat limited_expr =
  match limited_expr with
  | Over expr -> over expr
  | NotOver (expr, lim) ->
    (match expr with
     | Var x -> strat.on_var strat (not_over x lim)
     | Abs (x, b) -> strat.on_abs strat (not_over (x, b) lim)
     | App (f, x) -> strat.on_app strat (not_over (f, x) lim))
;;

let limited_strat_template eval =
  let rec loop = function
    | Over e -> over e
    | NotOver (e, lim) ->
      (match eval (not_over e lim) with
       | Over e -> over e
       | NotOver (e', lim') ->
         if lim = lim' (* to avoid getting stuck *)
         then not_over e' lim'
         else loop (not_over e' lim'))
  in
  let on_app _ = function
    | Over (f, x) -> over (app f x)
    | NotOver ((f, x), lim) -> loop (not_over (app f x) lim)
  in
  let on_abs _ = function
    | Over (x, b) -> over (abs x b)
    | NotOver ((x, b), lim) -> loop (not_over (abs x b) lim)
  in
  let on_var _ = function
    | Over x -> over (var x)
    | NotOver (x, lim) -> loop (not_over (var x) lim)
  in
  { on_var; on_abs; on_app }
;;

let ao_limited =
  let rec eval = function
    | Over expr -> over expr
    | NotOver (expr, lim) ->
      (match expr with
       | Var _ as l -> not_over l lim
       | Abs (x, b) ->
         (match eval (not_over b lim) with
          | Over b -> over (abs x b)
          | NotOver (b, lim) -> not_over (abs x b) lim)
       | App (f, arg) ->
         (match eval (not_over f lim) with
          | Over f -> over (app f arg)
          | NotOver (Abs (x, b), lim) ->
            (match eval (not_over arg lim) with
             | Over arg -> over (app f arg)
             | NotOver (arg, lim) ->
               let exp = subst x ~by:arg b in
               if lim < 1 then over (app (abs x b) arg) else not_over exp (lim - 1))
          | NotOver (f, lim) -> not_over (app f arg) lim))
  in
  limited_strat_template eval
;;

let cbn_limited =
  let rec eval = function
    | Over expr -> over expr
    | NotOver (expr, lim) ->
      (match expr with
       | (Var _ | Abs _) as e -> not_over e lim
       | App (f, arg) ->
         (match eval (not_over f lim) with
          | Over f -> over (app f arg)
          | NotOver (Abs (x, b), lim) ->
            let exp = subst x ~by:arg b in
            if lim < 1 then over (app (abs x b) arg) else not_over exp (lim - 1)
          | NotOver (f, lim) -> NotOver (app f arg, lim)))
  in
  limited_strat_template eval
;;

let set_lim expr lim = if lim < 1 then failwith "invalid value" else NotOver (expr, lim)
