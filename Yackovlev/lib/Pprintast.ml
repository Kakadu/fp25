[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let pp_binop fmt = function
  | Add -> Format.fprintf fmt "+"
  | Mul -> Format.fprintf fmt "*"
  | Sub -> Format.fprintf fmt "-"
  | Div -> Format.fprintf fmt "/"
;;

let pp_cmpop fmt = function
  | Eq -> Format.fprintf fmt "="
  | Neq -> Format.fprintf fmt "<>"
  | Lt -> Format.fprintf fmt "<"
  | Le -> Format.fprintf fmt "<="
  | Gt -> Format.fprintf fmt ">"
  | Ge -> Format.fprintf fmt ">="
;;

(* Flatten nested [Abs] into parameter list for [fun x y -> e] sugar *)
let rec collect_fun_params acc = function
  | Abs (x, body) -> collect_fun_params (x :: acc) body
  | body -> List.rev acc, body
;;

(* Precedence levels
   0: let, let rec, if, fun
   1: comparison operators
   2: + and -
   3: * and /
   4: application
   5: atomic expressions
*)
let rec pp_expr prec fmt = function
  | Var x -> Format.fprintf fmt "%s" x
  | Int n -> Format.fprintf fmt "%d" n
  | App (e1, e2) ->
    let my_prec = 4 in
    if prec > my_prec then Format.fprintf fmt "(";
    pp_expr my_prec fmt e1;
    Format.fprintf fmt " ";
    pp_expr (my_prec + 1) fmt e2;
    if prec > my_prec then Format.fprintf fmt ")"
  | Abs (x, body) as e ->
    let params, body = collect_fun_params [ x ] body in
    let my_prec = 0 in
    if prec > my_prec then Format.fprintf fmt "(";
    Format.fprintf fmt "fun";
    List.iter (fun p -> Format.fprintf fmt " %s" p) params;
    Format.fprintf fmt " -> ";
    pp_expr my_prec fmt body;
    if prec > my_prec then Format.fprintf fmt ")"
  | Binop (op, e1, e2) ->
    let my_prec =
      match op with
      | Mul | Div -> 3
      | Add | Sub -> 2
    in
    if prec > my_prec then Format.fprintf fmt "(";
    pp_expr my_prec fmt e1;
    Format.fprintf fmt " %a " pp_binop op;
    pp_expr (my_prec + 1) fmt e2;
    if prec > my_prec then Format.fprintf fmt ")"
  | Cmp (op, e1, e2) ->
    let my_prec = 1 in
    if prec > my_prec then Format.fprintf fmt "(";
    pp_expr my_prec fmt e1;
    Format.fprintf fmt " %a " pp_cmpop op;
    pp_expr (my_prec + 1) fmt e2;
    if prec > my_prec then Format.fprintf fmt ")"
  | Let (x, e1, e2) ->
    let my_prec = 0 in
    if prec > my_prec then Format.fprintf fmt "(";
    (* Desugar back [let f x y = body] when RHS is a lambda *)
    (match e1 with
     | Abs (p, body) ->
       let params, fun_body = collect_fun_params [ p ] body in
       Format.fprintf fmt "let %s" x;
       List.iter (fun p -> Format.fprintf fmt " %s" p) params;
       Format.fprintf fmt " = ";
       pp_expr my_prec fmt fun_body
     | _ ->
       Format.fprintf fmt "let %s = " x;
       pp_expr my_prec fmt e1);
    Format.fprintf fmt " in ";
    pp_expr my_prec fmt e2;
    if prec > my_prec then Format.fprintf fmt ")"
  | Let_rec (f, e1, e2) ->
    let my_prec = 0 in
    if prec > my_prec then Format.fprintf fmt "(";
    (match e1 with
     | Abs (p, body) ->
       let params, fun_body = collect_fun_params [ p ] body in
       Format.fprintf fmt "let rec %s" f;
       List.iter (fun p -> Format.fprintf fmt " %s" p) params;
       Format.fprintf fmt " = ";
       pp_expr my_prec fmt fun_body
     | _ ->
       Format.fprintf fmt "let rec %s = " f;
       pp_expr my_prec fmt e1);
    Format.fprintf fmt " in ";
    pp_expr my_prec fmt e2;
    if prec > my_prec then Format.fprintf fmt ")"
  | If (cond, then_, else_) ->
    let my_prec = 0 in
    if prec > my_prec then Format.fprintf fmt "(";
    Format.fprintf fmt "if ";
    pp_expr 1 fmt cond;
    Format.fprintf fmt " then ";
    pp_expr my_prec fmt then_;
    Format.fprintf fmt " else ";
    pp_expr my_prec fmt else_;
    if prec > my_prec then Format.fprintf fmt ")"
;;

let pp fmt e = pp_expr 0 fmt e
let pp_hum = pp
