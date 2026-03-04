[@@@ocaml.text "/*"]

(** Copyright 2026, [ChurinNick] *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let binop_to_string = function
  | Add -> " + "
  | Sub -> " - "
  | Mul -> " * "
  | Div -> " / "
;;

let cmpop_to_string = function
  | Eq -> " = "
  | Neq -> " <> "
  | Lt -> " < "
  | Le -> " <= "
  | Gt -> " > "
  | Ge -> " >= "
;;

let flag_to_string = function
  | Plain -> ""
  | Recursive -> "rec "
;;

let needs_parens = function
  | Lit _ | Var _ -> false
  | _ -> true
;;

let rec to_string_impl ?(parens = false) = function
  | Lit (Integer n) -> string_of_int n
  | Lit UnitVal -> "()"
  | Var name -> name
  | Lam (param, body) ->
    let body_str = to_string_impl body in
    if parens
    then "(fun " ^ param ^ " -> " ^ body_str ^ ")"
    else "fun " ^ param ^ " -> " ^ body_str
  | App (f, arg) ->
    let f_str =
      if needs_parens f then "(" ^ to_string_impl f ^ ")" else to_string_impl f
    in
    let arg_str = to_string_impl ~parens:true arg in
    if parens then "(" ^ f_str ^ " " ^ arg_str ^ ")" else f_str ^ " " ^ arg_str
  | Let (flag, name, bound, body) ->
    let bound_str = to_string_impl bound in
    let body_str = to_string_impl body in
    let let_str =
      "let " ^ flag_to_string flag ^ name ^ " = " ^ bound_str ^ " in " ^ body_str
    in
    if parens then "(" ^ let_str ^ ")" else let_str
  | Fix e ->
    let e_str =
      if needs_parens e then "(" ^ to_string_impl e ^ ")" else to_string_impl e
    in
    let fix_str = "fix " ^ e_str in
    if parens then "(" ^ fix_str ^ ")" else fix_str
  | UnOp (Negate, e) ->
    let e_str =
      if needs_parens e then "(" ^ to_string_impl e ^ ")" else to_string_impl e
    in
    let unop_str = "-" ^ e_str in
    if parens then "(" ^ unop_str ^ ")" else unop_str
  | BinOp (op, left, right) ->
    let left_str =
      if needs_parens left then "(" ^ to_string_impl left ^ ")" else to_string_impl left
    in
    let right_str =
      if needs_parens right
      then "(" ^ to_string_impl right ^ ")"
      else to_string_impl right
    in
    let op_str = binop_to_string op in
    let binop_str = left_str ^ op_str ^ right_str in
    if parens then "(" ^ binop_str ^ ")" else binop_str
  | CmpOp (op, left, right) ->
    let left_str =
      if needs_parens left then "(" ^ to_string_impl left ^ ")" else to_string_impl left
    in
    let right_str =
      if needs_parens right
      then "(" ^ to_string_impl right ^ ")"
      else to_string_impl right
    in
    let op_str = cmpop_to_string op in
    let cmp_str = left_str ^ op_str ^ right_str in
    if parens then "(" ^ cmp_str ^ ")" else cmp_str
  | If (cond, then_branch, else_branch) ->
    let cond_str = to_string_impl cond in
    let then_str = to_string_impl then_branch in
    let else_str = to_string_impl else_branch in
    let if_str = "if " ^ cond_str ^ " then " ^ then_str ^ " else " ^ else_str in
    if parens then "(" ^ if_str ^ ")" else if_str
;;

let to_string ?(parens = false) expr = to_string_impl ~parens expr
let print ?(parens = false) expr = print_endline (to_string ~parens expr)
let print_with_parens expr = print ~parens:true expr
let print_simple expr = print ~parens:false expr
