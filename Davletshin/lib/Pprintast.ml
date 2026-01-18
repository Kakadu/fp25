[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Pretty printer goes here *)

open Ast
open Utils

let pp ?(compact = true) =
  let open Format in
  let mangle t fmt x =
    if is_free_in x t || not compact then fprintf fmt "%s" x else fprintf fmt "_"
  in
  let rec pp fmt = function
    | Int n -> Format.fprintf fmt "%d" n
    | Var s -> Format.fprintf fmt "%s" s
    | App (l, r) -> Format.fprintf fmt "(%a %a)" pp l pp r
    | Abs (x, t) -> Format.fprintf fmt "(fun %a -> %a)" (mangle t) x pp t
    | Binop (op, l, r) ->
      let bop =
        match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Divide -> "/"
        | Eq -> "="
        | Neq -> "<>"
        | Lt -> "<"
        | Gt -> ">"
        | Le -> "<="
        | Ge -> ">="
      in
      Format.fprintf fmt "(%a %s %a)" pp l bop pp r
    | Unop (op, e) ->
      let uop =
        match op with
        | Pos -> "+"
        | Neg -> "-"
      in
      Format.fprintf fmt "(%s%a)" uop pp e
    | If (c, t, e) -> Format.fprintf fmt "if %a then %a else %a" pp c pp t pp e
  in
  pp
;;

let pp_hum = pp ~compact:true
let pp = pp ~compact:false
