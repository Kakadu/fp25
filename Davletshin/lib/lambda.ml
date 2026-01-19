(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Utils

let bop_err = "TODO: Fix it"
let uop_err = "TODO: Fix it"
let if_guard_err = "TODO: Fix it"

(* Smart constructors *)
let int n = Int n
let var x = Var x
let abs x y = Abs (x, y)
let app x y = App (x, y)
let binop op l r = Binop (op, l, r)
let unop op e = Unop (op, e)
let if_cond c t e = If (c, t, e)
let let_bind f n e1 e2 = Let (f, n, e1, e2)

let replace_name x ~by =
  let rec helper = function
    | Int n -> int n
    | Var y when String.equal x y -> var by
    | Var t -> var t
    | App (l, r) -> app (helper l) (helper r)
    | Abs (y, t) when String.equal x y -> abs by (helper t)
    | Abs (z, t) -> abs z (helper t)
    | Binop (op, l, r) -> binop op (helper l) (helper r)
    | Unop (op, e) -> unop op (helper e)
    | If (c, t, e) -> if_cond (helper c) (helper t) (helper e)
    | Let (f, n, e1, e2) when String.equal x n -> let_bind f n (helper e1) e2
    | Let (f, n, e1, e2) -> let_bind f n (helper e1) (helper e2)
  in
  helper
;;

let rec next_name s old =
  if List.mem ~equal:String.equal old s then next_name ("_" ^ s) old else s
;;

let is_value = function
  | Int _ -> true
  | _ -> false
;;

(**  The call [subst x ~by:v e] means `[x/v]e` or `e[v -> x]` *)
let subst x ~by:v =
  let rec helper = function
    | Int n -> int n
    | Var y when String.equal y x -> v
    | Var y -> var y
    | App (l, r) -> app (helper l) (helper r)
    | Abs (y, b) when String.equal y x -> abs y b
    | Abs (y, t) when is_free_in y v ->
      let frees = free_vars v @ free_vars t in
      let w = next_name y frees in
      helper (abs w (replace_name y ~by:w t))
    | Abs (y, b) -> abs y (helper b)
    | Binop (op, l, r) -> binop op (helper l) (helper r)
    | Unop (op, e) -> unop op (helper e)
    | If (c, t, e) -> if_cond (helper c) (helper t) (helper e)
    | Let (f, n, e1, e2) when String.equal x n -> let_bind f n (helper e1) e2
    | Let (f, n, e1, e2) when is_free_in n v ->
      let frees = free_vars v @ free_vars e2 in
      let w = next_name n frees in
      helper (let_bind f w (helper e1) (replace_name n ~by:w e2))
    | Let (f, n, e1, e2) -> let_bind f n (helper e1) (helper e2)
  in
  helper
;;

(* Call-by-Value Reduction to Weak Normal Form *)
(** понятно что let rec нужно реализовать *)
let rec cbv_strat = function
  | Int n -> int n
  | Var name -> var name
  | Abs (x, b) -> abs x b
  | App (l, r) ->
    let on_app f arg =
      match cbv_strat f with
      | Abs (x, e) ->
        let arg2 = cbv_strat arg in
        cbv_strat @@ subst x ~by:arg2 e
      | f2 -> app f2 (cbv_strat arg)
    in
    on_app l r
  | Binop (op, l, r) -> eval_bop op l r
  | Unop (op, e) -> eval_uop op e
  | If (c, t, e) -> eval_if c t e
  | Let (_, n, e1, e2) -> subst n ~by:(cbv_strat e1) e2 |> cbv_strat

and eval_bop bop l r =
  match bop, cbv_strat l, cbv_strat r with
  | Plus, Int a, Int b -> int (a + b)
  | Minus, Int a, Int b -> int (a - b)
  | Times, Int a, Int b -> int (a * b)
  | Divide, Int a, Int b -> int (a / b)
  | Eq, Int a, Int b -> int (if a = b then 1 else 0)
  | Neq, Int a, Int b -> int (if a <> b then 1 else 0)
  | Lt, Int a, Int b -> int (if a < b then 1 else 0)
  | Gt, Int a, Int b -> int (if a > b then 1 else 0)
  | Le, Int a, Int b -> int (if a <= b then 1 else 0)
  | Ge, Int a, Int b -> int (if a >= b then 1 else 0)
  | _ -> failwith bop_err

and eval_uop op e =
  match op, cbv_strat e with
  | Pos, Int a -> int (+a)
  | Neg, Int a -> int (-a)
  | _ -> failwith uop_err

and eval_if c t e =
  match cbv_strat c with
  | Int 0 -> cbv_strat e
  | Int _ -> cbv_strat t
  | _ -> failwith if_guard_err
;;

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
